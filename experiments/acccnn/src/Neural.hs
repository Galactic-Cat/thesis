module Neural () where
    import Data.Array.Accelerate as A
    import qualified Prelude as P ((*), foldl, IO, map, max, sum, zipWith)
    import System.Random (mkStdGen, Random (randomIO, randoms))

    import Matrix (mvm)

    type Layer = Acc (Vector Float)
    type Layer2D = Acc (Matrix Float)
    data Pack = Pack [Layer] | Pack2D [Layer2D]
    data WeightedPack = WeightedPack [(Layer, Matrix Float)] | WeightedPack2D [(Layer2D, Matrix Float)]

    convolve :: Layer2D -> Matrix Float -> Layer2D
    convolve layer kernel = stencil conv zeroPadding layer
        where conv = convolve3x3 kernel

    convolve3x3 :: Matrix Float -> Stencil3x3 Float -> Exp Float
    convolve3x3 kernel ((tl, tc, tr),
                        (cl, cc, cr),
                        (bl, bc, br)) = P.sum $ P.zipWith (P.*) (P.map lift $ toList kernel) stencilList
        where stencilList   = [tl, tc, tr, cl, cc, cr, bl, bc, br]

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

    newKernel :: Int -> Int -> [Float] -> Matrix Float
    newKernel h w = fromList (Z :. h :. w)

    --region Util
    relu :: Exp Float -> Exp Float
    relu = max (constant 0.0)

    zeroPadding :: Boundary (Matrix Float)
    zeroPadding = function $ \_ -> constant 0.0
    --endregion

    --region Randomness
    randomList :: Int -> [Float]
    randomList seed = randoms (mkStdGen seed)

    randomSeed :: P.IO Float
    randomSeed = randomIO
    --endregion