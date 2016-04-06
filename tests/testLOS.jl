using Base.Test

include("../LOS.jl")

handler(r::Test.Success) = nothing
handler(r::Test.Failure) = error("Error on custom handler: $(r.expr)")
handler(r::Test.Error) = rethrow(r)

function test() 
  Test.with_handler(handler) do
    test_index()
    test_index_unindex()
    test_update_const()
    println("All tests passing")
  end
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
