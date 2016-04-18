using Base.Test

include("../LOS.jl")


function test() 
  test_index()
  test_index_unindex()
  test_update_const()
  test_compute_swap()
  test_extend()
  test_compute_swap_operator()
  println("All tests passing")
end

function test_update_const() 
  dims = [2,2]
  k = 1
  c = 2
  expected = 
  [ 0 0 1 0
    0 0 0 1
    0 0 1 0
    0 0 0 1]
  @test U_xk_c(dims, k, c) == expected
end

function test_index_unindex() 
  dims = [13,17,2,1,22,3,7]
  d = prod(dims)
  for i=1:d
    @test index(dims, unindex(dims, i)) == i
  end
end

function test_index()
  dims = [2,2]
  values = [1,1]
  @test index(dims, values) == 1
  values = [1,2]
  @test index(dims, values) == 2
  values = [2,1]
  @test index(dims, values) == 3
  values = [2,2]
  @test index(dims, values) == 4

  dims = [3,5,2]
  values = [1,4,2]
  @test index(dims, values) == 8
  values = [2,5,1]
  @test index(dims, values) == 19
  values = [3,1,2]
  @test index(dims, values) == 22 
end

function test_compute_swap()
  a = [2,4,6,7]
  @test compute_swaps(a) == [7,2,6,4]

  b = [1,2,3,4]
  @test compute_swaps(b) == [1,2,3,4]

  c = [8,1,4,5]
  @test compute_swaps(c) == [1,8,5,4]
end

function test_extend() 
  values_restricted = [1,3,2,1]
  ordinals = [7,2,6,4]
  n = 8
  expected = [1,3,1,1,1,2,1,1]
  @test extend(values_restricted,ordinals,n) == expected

  values_restricted = [3,2,5,2]
  ordinals = [1,5,3,4]
  n = 6
  expected = [3,1,5,2,2,1]
  @test extend(values_restricted,ordinals,n) == expected
end

function test_compute_swap_operator()
  dims = [2,2,2]
  expected = E(8,1,1) + E(8,2,3) + E(8,3,2) + E(8,4,4) +
             E(8,5,5) + E(8,6,7) + E(8,7,6) + E(8,8,8)
  @test compute_swap_operator(dims,2,3) == expected

  expected = E(8,1,1) + E(8,2,5) + E(8,3,3) + E(8,4,7) +
             E(8,5,2) + E(8,6,6) + E(8,7,4) + E(8,8,8)
  @test compute_swap_operator(dims,1,3) == expected
end
