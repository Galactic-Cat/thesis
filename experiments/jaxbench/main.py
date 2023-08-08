from functools import partial
import jax
import jax.numpy as jnp
import time

def f(x):
    return jnp.sin(x)

# Wrap f with vmap
f_vmap = jax.pmap(f)

# Define a new function that sums the elements of the array returned by f_vmap
def f_sum(x):
    return jnp.sum(f_vmap(x))

# Wrap f_sum with grad
f_sum_grad = jax.grad(f_sum)

batch_sizes = [10, 50, 100, 500, 1000, 5000, 10000, 50000, 100000, 500000, 1000000, 5000000]

time.sleep(1)

for batch in batch_sizes:
    vmap_time = 0
    grad_time = 0
    print(f'\nBatch {batch}:')

    for _ in range(1000):
        # Now let's create some large input data
        x = jnp.linspace(0, 100000, batch)

        # Benchmarking the vmap function
        start_time = time.time()
        result_vmap = f_vmap(x).block_until_ready()  # block_until_ready is used to ensure computation is finished
        # f_vmap._clear_cache()
        vmap_time += time.time() - start_time

        # Benchmarking the grad function
        start_time = time.time()
        result_grad = f_sum_grad(x).block_until_ready()  # block_until_ready is used to ensure computation is finished
        # f_sum_grad._clear_cache()
        grad_time += time.time() - start_time

    print(f'  vmap execution time: {vmap_time}, average: {vmap_time / 1000}')
    print(f'  grad execution time: {grad_time}, average: {grad_time / 1000}')
