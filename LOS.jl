# LOS.jl

#=
 This file contains functions used by julia files generated by pWhile 
 compiler. Most of the naming conventions used in this file are 
 inspired by the paper `Probabilistic Semantics and Program Analysis` 
 by A.D.Pierro, C.Hankin and H.Wiklicky. Some references to this paper
 are also made throught the comments in this code.
 
=#

#--------------------------------------------------------------------
# Auxiliary functions
#--------------------------------------------------------------------

"""
    I(dim)  

Return sparse identity matrix of dimension `dim`
"""
function I(dim)
  return speye(Int,dim)
end

"""
    E(m, n, i, j)

Return sparse `m` by `n` matrix with `ij`-entry 1, others 0
"""
function E(m, n, i, j)
  @assert valid_index(m, i) && valid_index(n, j)
  R = spzeros(Int,m,n)
  R[i,j] = 1
  return R
end

"""
    E(dim, i, j)

Return sparse square matrix with `ij`-entry 1, others 0, dimension `dim`
"""
function E(dim, i, j)
  @assert valid_index(dim, i) && valid_index(dim, j)
  R = spzeros(Int,dim, dim)
  R[i,j] = 1
  return R
end

"""
    E(dim, i)

Return sparse matrix with `i`th diagonal entry 1, others 0, dimension `dim`
"""
function E(dim, i)
  return E(dim, i , i)
end

"""
    U_c(dim,c)

Return sparse square matrix of dimension `dim` with entries
in `c`th column 1, otherwise 0
"""
function U_c(dim,c) 
  @assert valid_index(dim, c)
  R = spzeros(Int,dim,dim)
  for i = 1:dim
    R[i,c] = 1
  end
  return R
end

"""
    e_i(dim,i)

Return a vector of dimension `dim` (ie a 1x`dim` matrix) with a one at 
position `i`, zeros otheriwse
"""
function e_i(dim,i)
  @assert valid_index(dim, i)
  R = spzeros(Int,1,dim)
  R[i] = 1
  return R
end

"""
    findall(lookedfor, target)

Find all indices of elements from 'lookedfor' in 'target'

# Examples
```julia 
julia> findall([1,3], [1,2,3,4])
2-element Array{Any,1}:
 1
 3

julia> findall([2,3,4],[1,3,5])
1-element Array{Any,1}:
 2
```
"""
# NOT USED CURRENTLY - USED ONLY WHEN Ur USED
function findall(lookedfor, target)
  @assert issorted(lookedfor) && issorted(target)
  i, j = 1, 1
  result = []
  while i <= length(lookedfor) && lookedfor[i] <= target[end]
    if lookedfor[i] > target[j]
      j += 1
    elseif lookedfor[i] < target[j]
      i += 1
    else 
      push!(result, j)
      i += 1
      j += 1
    end
  end
  result
end


#=
State of variables in a program can be represented twofold.
Firstly, it could be an array specifying value of each variable
(i.e. value of ith variable is at position i in the array) - this 
is the representation described in the paper.
Alternatively, it could be represented by a single integer value 
between 1 and d (inclusive), where d is the number of different states.
Now clearly there's an isomorphism between the two representations.

# Example
Suppose there are two variables, x and y, both can take 2 different values
(it doesn't matter what those values are). Then the isomorphism is:
```julia
                        [1,1] <-> 1
                        [1,2] <-> 2
                        [2,1] <-> 3
                        [2,2] <-> 4
```
index and unindex provide the mapping between the two representations.
  - index tranforms state represented by array of values of variables into
    state represented by an integer 
  - unindex does the opposite
=#

"""
    unindex(dims, i)

Return array representation of the state of the variables corresponding
to the integer representation `i`

# Example
```julia
julia> unindex([2,2], 3)
2-element Array{Int64,1}:
 2
 1
```
"""
function unindex(dims::Array{Int,1}, i::Int) 
  @assert i <= prod(dims)
  result::Array{Int,1} = []
  if length(dims) > 1
    result = unindex(dims[1:end-1] ,div(i - 1, dims[end]) + 1)
  end
  push!(result, rem(i - 1, dims[end]) + 1)
  return result
end

"""
    index(dims, values)

Return integer representation of the state of the variables corresponding
to the array representation `values`

# Example
```julia
julia> index([2,2], [2,1])
 3
```
"""
function index(dims::Array{Int,1}, values::Array{Int,1})
  @assert length(dims) == length(values)
  for i=1:length(dims)
    @assert dims[i] >= values[i]
  end
  if length(dims) == 1
    return values[1]
  else 
    return (values[1] - 1) * prod(dims[2:end]) + index(dims[2:end], values[2:end])
  end
end

"""
    valid_index(len, i)

Check whether `i` is a valid index into a vector of length `len`

# Example
```julia
julia> valid_index(5,2)
true 

julia> valid_index(5,0)
false
```
"""
function valid_index(len, i)
  return i > 0 && i <= len
end

"""
    swap!(arr, i, j)

Swaps elements at positions `i` and `j` in an array `arr`, in place.
Assumes `i` and `j` are valid indexes into the array.

# Example
```julia
julia> arr = [1,2,3,4,5]
julia> swap!(arr,2,4)
julia> arr
5-element Array{Int64,1}:
 1
 4
 3
 2
 5
```
"""
function swap!(arr, i, j)
  @assert valid_index(length(arr), i) && valid_index(length(arr), j)
  if i != j
    arr[i], arr[j] = arr[j], arr[i] 
  end
end

"""
    compute_swaps(ordinals)

Given `ordinals` (of variables that appear in an arithmetic expression)
compute how variables need to be swapped to minimise the number of swaps.
Using the fact that variables will be swapped into positions 1,2,...,n, 
where n is length of `ordinals`, the swaps are represented as an array 
of length n. 

# Example
```julia
julia> compute_swaps([7,1,6,4])
4-element Array{Int64,1}:
 1
 7
 6
 4
```
Here the resulting array represents the swaps:
  2 <=> 7
  3 <=> 6

1 and 4 don't need to be swapped since they belong to the first 4 ordinals
"""
function compute_swaps(ordinals::Array{Int,1})
  result = sort(union(ordinals))
  n = length(result)
  for i = n:-1:1
    if result[i] <= n && result[i] != i
      swap!(result, i, result[i])
    end
  end
  return result
end

"""
    compute_swapped_dims(dims, swaps)

Given variable dimensions in the intial order `dims`, compute
the dimensions order after the swap represented by `swaps`
(so returned value is a permutation of `dims`)

# Example
```julia
julia> compute_swapped_dims([3,3,2,2], [3,4])
4-element Array{Int64,1}:
 2
 2
 3
 3
```
"""
function compute_swapped_dims(dims::Array{Int,1}, swaps::Array{Int,1})
  result = copy(dims)
  for i = 1:length(swaps)
    swap!(result, i, swaps[i])
  end
  return result
end

"""
    extend(values_restricted, ordinals, n)

Extend the restricted vector of variables values `values_restricted`
to a full vector of length `n`, where the values of variables with
ordinals in `ordinals` are specified in `values_restricted`. Other
values are set to 1.

# Example
```julia
julia> extend([3,2,4],[1,5,4],6)
6-element Array{Int64,1}:
 3
 1
 1
 4
 2
 1
```
"""
function extend(values_restricted::Array{Int,1}, ordinals::Array{Int,1}, n::Int) 
  for i in ordinals
    @assert i <= n
  end
  @assert length(values_restricted) == length(ordinals)
  result = squeeze(ones(Int,1,n),1)
  for i = 1:length(ordinals)
    result[ordinals[i]] = values_restricted[i]
  end
  return result
end

"""
    compute_swap_operator(dims, p, q)

Compute operator (i.e. matrix) that swaps `p`th and `q`th variables
It is based on an idea of a commutator matrix which makes Kronecker
product commute. 

"""
function compute_swap_operator(dims::Array{Int,1}, p::Int, q::Int)
  @assert valid_index(length(dims), p) && valid_index(length(dims), q)
  d = prod(dims)
  if p == q
    return I(d)
  end
  R = spzeros(Int,d,d)
  for i = 1:dims[p]
    for j = 1:dims[q]
      A = I(1)
      for k = 1:p-1
        A = kron(A,I(dims[k]))
      end
      A = kron(A,E(dims[p],dims[q],i,j))
      for k = p+1:q-1
        A = kron(A,I(dims[k]))
      end
      A = kron(A,transpose(E(dims[p],dims[q],i,j))) 
      for k = q+1:length(dims)
        A = kron(A,I(dims[k]))
      end
      R += A
    end
  end
  return R
end

"""
    compute_swaps_operator(dims, swaps)

Compute operator (i.e. matrix) that swaps variables as represented by
`swaps` vector. It is the product of swap operators for each individual 
swap

"""
function compute_swaps_operator(dims::Array{Int,1}, swaps::Array{Int,1})
  R = I(prod(dims))
  for i = 1:length(swaps)
    if i != swaps[i]
        R = R * compute_swap_operator(dims, i, swaps[i])
    end
  end
  return R
end

#--------------------------------------------------------------------
# State update operators and filter operators
#--------------------------------------------------------------------
# 
# In all fuctions below dims is the vector of variable dimensions
#

"""
    U_xk_c(dims, k, c)

Compute operator that assigns `k`th variable value `c` 
(i.e. the `c`th value in its range), and leaves others
as they are

"""
function U_xk_c(dims::Array{Int,1}, k::Int, c::Int) 
  @assert c > 0 && c <= dims[k]
  R = I(1)
  for i = 1:(k - 1)
    R = kron(R, I(dims[i]))
  end
  R = kron(R, U_c(dims[k],c))
  for i = (k + 1):length(dims)
    R = kron(R, I(dims[i]))
  end
  return R
end

"""
    U_e(dims, ordinal, ordinals, update)

Compute the operator that assigns to variable with ordinal `ordinal` 
the value of expression e, where the value, given values of variables,
is computed using the function `update`. `ordinals` is the vector of 
ordinals of variables that appear on the RHS of the assignment.
With that, the operator can be computed more efficiently by
swapping involved variables so they come first, compute the smaller
update operator restricted to involved variables and compute 
Kronecker product of that operator with identity matrices representing
lack of influence of other variables on the current assignment.

"""
function Ue(dims::Array{Int,1}, ordinal::Int, ordinals::Array{Int,1}, update::Function)
  if length(ordinals) == 0
    return Ue(dims, ordinal, update)
  end
  swaps = compute_swaps(push!(copy(ordinals), ordinal))
  n = length(swaps)
  dims_swapped = compute_swapped_dims(dims, swaps)
  dims_restricted = dims_swapped[1:n]
  newordinal = findfirst(swaps, ordinal)
  d = prod(dims_restricted)
  R = spzeros(d,d)
  for i = 1:d
    values_restricted = unindex(dims_restricted, i)
    values = extend(values_restricted, swaps, length(dims))
    values_restricted[newordinal] = update(values)
    if values_restricted[newordinal] > 0
      R[i, index(dims_restricted, values_restricted)] = 1
    end
  end
  for dim in dims_swapped[n+1:end]
    R = kron(R,I(dim))
  end
  K = compute_swaps_operator(dims, swaps)
  return K * R * transpose(K) 
end

"""
    U_e(dims, ordinal, update)

Compute the operator that assigns to variable with ordinal `ordinal` 
the value of expression e, where the value, given values of variables,
is computed using the function `update`. This is the basic implementation
which doesn't take into account the number of variables appearing on 
the RHS of the assignment. 
It loops over all the possible combinations of variable values (and
uses `unindex` to recover those) and computes the value of the 
expression (using the `update` function) for those values of variables.

"""
function Ue(dims::Array{Int,1}, ordinal::Int, update::Function)
  @assert ordinal <= length(dims)
  d = prod(dims)
  R = spzeros(d,d)
  for i = 1:d
    values = unindex(dims, i)
    values[ordinal] = update(values) # might be that the value computed here
                                     # falls out of range (so is 0)
    if values[ordinal] > 0
      R[i,index(dims, values)] = 1
    end
  end
  return R
end

function Ur(dims::Array{Int,1}, ordinal::Int, 
           range::Array{Int,1}, assign_range::Array{Int,1})
  d = prod(dims)
  values = findall(assign_range, range)
  result = spzeros(d,d)
  for i in values 
    result += Uc(dims, ordinal, i)
  end
  return 1//length(values) * result
end

function P(dims::Array{Int,1}, values::Array{Int,1}) 
  @assert length(dims) == length(values)
  R = I(1)
  for i = 1:length(dims)
    R = kron(R, E(dims[i], values[i]))
  end
  return R
end

function P(dims::Array{Int,1}, test::Function, c::Bool)
  d = prod(dims)
  R = spzeros(d,d)
  for i = 1:d
    values = unindex(dims, i)
    if test(values) == c
      R += P(dims, values)
    end
  end
  return R
end

1;
