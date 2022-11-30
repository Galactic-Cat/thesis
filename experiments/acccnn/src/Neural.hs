module Neural (CNN (NewCNN), createCNN) where
    import Data.Array.Accelerate as A
    import qualified Prelude as P ((*), (==), drop, head, foldl, fst, IO, map, max, return, snd, sum, zipWith)
    import System.Random (mkStdGen, Random (randomIO, randoms))

    import Matrix (mvm)

    type Layer = Acc (Vector Float)
    type Layer2D = Acc (Matrix Float)
    data CNN =
        NewCNN
        (Int, Int)    -- Input size
        [(Int, Bool)] -- Convolutional layers:   [(stride, do max-pooling)]
        [Int]         -- Fully connected layers: [size of next layer]
        |
        CNN
        [Layer2D -> Matrix Float -> Layer2D]          -- Convolutional layers
        [Matrix Float]                                -- Convolutional layer weights
        [Layer -> Matrix Float -> Exp Float -> Layer] -- Fully connected layers
        [(Float, Matrix Float)]                       -- Fully connected layer weights: (bias, actual weights)

    --region Create operations
    createC :: (Int, Int) -> [(Int, Bool)] -> [Float] -> [(Layer2D -> Matrix Float -> Layer2D, Matrix Float)]
    createC _               []                     _   = []
    createC (height, width) ((stride, pooling):ls) rds =
        let
            mat = fromList (Z :. 3 :. 3) rds -- 3x3 kernel
            fun lyr krl =
                if   pooling
                then forceStride (maxPooling (convolve lyr krl 0)) stride
                else convolve lyr krl stride
            newdim = (div height stride, div width stride)
        in
            (fun, mat) : createC newdim ls (P.drop (height * width) rds)

    createCNN :: CNN -> P.IO CNN
    createCNN v@CNN{} = P.return v
    createCNN (NewCNN inp cls fls) =
        do
            seed       <- randomSeed -- Randomness introduces IO
            let createdC        = createC inp cls (randomList seed)
                cnnLayers       = P.map P.fst createdC
                cnnWeights      = P.map P.snd createdC
                (Z :. lr :. lc) = arrayShape $ last cnnWeights
                createdF        = createF (lr * lc) fls (randomList seed)
                fcLayers        = P.map P.fst createdF
                fcWeights       = P.map P.snd createdF
            
            P.return $ CNN cnnLayers cnnWeights fcLayers fcWeights
        where
            last :: [Matrix Float] -> Matrix Float
            last []     = fromList (Z :. (0 :: Int) :. (0 :: Int)) [2]
            last [x]    = x
            last (_:xs) = last xs

    createF :: Int -> [Int] -> [Float] -> [(Layer -> Matrix Float -> Exp Float -> Layer, (Float, Matrix Float))]
    createF _ []     _   = []
    createF l (n:ns) rds =
        let
            bias = P.head rds
            mat = fromList (Z :. l :. n) (P.drop 1 rds)
            fun lyr wgt bia = fullConnect lyr (use wgt) bia relu
        in
            (fun, (bias, mat)) : createF n ns (P.drop (l * n + 1) rds)
    --endregion

    --region Layer operations
    convolve :: Layer2D -> Matrix Float -> Int -> Layer2D
    convolve layer kernel stride = 
        let
            newLayer = stencil conv zeroPadding layer
        in
            if
                stride P.== 0
            then
                newLayer
            else
                forceStride newLayer stride
        where
            conv = convolve3x3 kernel

    convolve3x3 :: Matrix Float -> Stencil3x3 Float -> Exp Float
    convolve3x3 kernel ((tl, tc, tr),
                        (cl, cc, cr),
                        (bl, bc, br)) = P.sum $ P.zipWith (P.*) (P.map lift $ toList kernel) stencilList
        where stencilList   = [tl, tc, tr, cl, cc, cr, bl, bc, br]

    forceStride :: Layer2D -> Int -> Layer2D
    forceStride layer stride = generate newshape collect
        where 
            collect (I2 r c) = layer ! I2 (r * constant stride) (c * constant stride)
            I2 pr pc = shape layer
            newshape = I2 (div pr $ constant stride) (div pc $ constant stride)

    fullConnect :: Layer -> Acc (Matrix Float) -> Exp Float -> (Exp Float -> Exp Float) -> Layer
    fullConnect layer weights bias activation =
        let
            weighted = fold (+) 0.0 $ mvm weights layer
            biased = map (+ bias) weighted
        in 
            map activation biased

    maxPooling :: Layer2D -> Layer2D
    maxPooling = stencil pool zeroPadding
        where pool :: Stencil3x3 Float -> Exp Float
              pool ((tl, tc, tr),
                    (cl, cc, cr),
                    (bl, bc, br)) = P.foldl P.max (constant 0) [tl, tc, tr, cl, cc, cr, bl, bc, br] -- NOTE: Max with default 0, maybe wrong for NN with weights in (-1, 1)
    --endregion

    --region Util
    relu :: Exp Float -> Exp Float
    relu = max (constant 0.0)

    zeroPadding :: Boundary (Matrix Float)
    zeroPadding = function $ \_ -> constant 0.0
    --endregion

    --region Randomness
    randomList :: Int -> [Float]
    randomList seed = randoms (mkStdGen seed)

    randomSeed :: P.IO Int
    randomSeed = randomIO
    --endregion