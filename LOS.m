1;

# New version spkron -> kron (c) 2012
# New version cellval = access array elemts, to avoid error (c) 2014
# New version include normal(isation)

#---------------------------------------------------------------------

function pn = normal(p,context)
  psum = sum(context);
  if psum>0
    pn = abs(p/psum);
  else
    pn = 0.0;
  endif
endfunction
 
#---------------------------------------------------------------------
# Identity matrix of dimension d

function M = Id(d) 
  M = speye(d,d);
endfunction

#-----------------------------------
# Matrix Unit of dimension d with E_{nm} = 1

function M = E(d,n,m)
  M = spalloc(d,d);
  M(n,m) = 1;
endfunction

#---------------------------------------------------------------------
# Base vector of dimension d with d_n = 1

function x = b(d,n)
  x = spalloc(1,d);
  x(1,n) = 1;
endfunction

#-----------------------------------
# Initial vector with state cs (dimensions in ds)

function x = init(ds,cs)
  v = length(ds); # same as length cs !!
  x = 1;
  for i=1:v
    x = kron(x,b(ds(i),cs(i)));
  endfor
endfunction

#-----------------------------------
# Initial vector with state cs (dimensions in ds, s steps)

function x = initial(ds,s,cs)
  x = kron(init(ds,cs),b(s,1));
endfunction

#---------------------------------------------------------------------
# Access i="i"th element in array a="k[1]" for store v
# Needed to fix some parsing[?] errors in octave

function r = cellval (a,i,v)
  aidx = name2idx(a);
  oidx = name2idx(i);
  aval = idx2rng(aidx);
  ival = idx2rng(oidx);
  r = aval(v(oidx+(ival(v(oidx))-1)));
endfunction 

#---------------------------------------------------------------------
# Test variable equal to c (dimension d)

function M = Pc(d,c)
  M = spalloc(d,d);
  M(c,c) = 1;
endfunction
 
#-----------------------------------
# Test state for equality of state cs (dimensions in ds)

function M = Ps(ds,cs)
  v = length(ds); # same as length cs !!
  M = 1;
  for i=1:v
    M = kron(M,Pc(ds(i),cs(i)));
  endfor
endfunction
 
#-----------------------------------
# Test for variable x_k equal to c (dimensions in ds)

function M = Px(ds,k,c)
  v = length(ds); 
  M = 1;
  for i=1:k-1
    M = kron(M,Id(ds(i)));
  endfor
  M = kron(M,Pc(ds(k),c));
  for i=k+1:v
    M = kron(M,Id(ds(i)));
  endfor
endfunction

#-----------------------------------
# Test for equality between expression e and c

function M = Pe(ds,e,c)
  v = length(ds); # same as length cs !!  
  d = prod(ds);
  M = spalloc(d,d);
  for i=1:d
    s = unindex(ds,i);
    if (feval(e,s) == c)
      M = M + Ps(ds,s);
    endif
  endfor
endfunction

#---------------------------------------------------------------------
# Set to constant c (dimension d)

function M = Uc(d,c)
  M = spalloc(d,d);
  for i=1:d
    M(i,c) = 1;
  endfor
endfunction

#-----------------------------------
# Set to state cs (dimensions in ds)

function M = Us(ds,cs)
  v = length(ds); # same as length cs !!
  M = 1;
  for i=1:v
    M = kron(M,Uc(ds(i),cs(i)));
  endfor
endfunction

#-----------------------------------
# Set variable x_k to value c (dimensions in ds)

function M = Ux(ds,k,c)
  v = length(ds); 
  M = 1;
  for i=1:k-1
    M = kron(M,Id(ds(i)));
  endfor
  M = kron(M,Uc(ds(k),c));
  for i=k+1:v
    M = kron(M,Id(ds(i)));
  endfor
endfunction

#-----------------------------------
# Set variable x_k to expr e (dimensions in ds)

function M = Ue(ds,k,e)
  d = prod(ds);
  M = spalloc(d,d);
  for c=1:ds(k)
    M = M + Pe(ds,e,c)*Ux(ds,k,c);
  endfor
endfunction

#-----------------------------------
# Array assignment to a[1]=x_k..x_{k+l-1} with i = index expr, c = const

function M = Ui(ds,i,k,l,c)
  d = prod(ds);
  M = spalloc(d,d);
  for j=1:l
    M = M + Pe(ds,i,j)*Ux(ds,k+j-1,c);
  endfor
end

#-----------------------------------
# Array assignment to a[1]=x_k..x_{k+l-1} with i = index expr, e = rhs

function M = Ua(ds,i,k,l,e)
  d = prod(ds);
  M = spalloc(d,d);
  for j=1:l
    M = M + Pe(ds,i,j)*Ue(ds,k+j-1,e);
  endfor
end

#-----------------------------------
# Pointer assignment 

function M = Up(ds,r,k,e)
  v = length(ds); 
  d = prod(ds);
  M = spalloc(d,d);
  if (r==0)
    M = Ue(ds,k,e);
  else
    for i=1:v
      M = M + Px(ds,k,i)*Up(ds,r-1,i,e);
    endfor
  endif
endfunction

#---------------------------------------------------------------------
# Based on timing for init state compute (potential) transition times
#---------------------------------------------------------------------

function T = Times (d,t)
  b = length(t);
  for i=1:b
    BT(:,i) = t;
  endfor;
  T = kron(ones(d,d),BT);
endfunction;

#---------------------------------------------------------------------
# For Tensor Products: Partial Indices <-> Global Index
#---------------------------------------------------------------------
# i = (i_1 - 1)d_2 + (i_2 - 1) + 1
# (i-1) div d_2 = i_1 - 1
# (i-1) mod d_2 = i_2 - 1
#---------------------------------------------------------------------
# ds = dimension array, cs = factor index array -> global tensor index

function idx = index(ds,cs)
  v = length(ds); # same as length cs !!
  if (v==1)
    idx = cs(1);
  else
    idx = (cs(1)-1)*prod(ds(2:v)) + index(ds(2:v),cs(2:v));
  endif
endfunction

#-----------------------------------
# ds = dimension array, idx = global tensor index -> factor index array
# unindex recovers the values of each variable corresponding to idx^th 
# configuration of values of all variables
# one can think of unindex as inverse of index

function cs = unindex(ds,idx)
  v = length(ds);
  if (v==1)
    cs(1) = idx;
  else
    d      = prod(ds(2:v));
    i      = 1 + rem(idx-1,d);     # mod
    cs(1)  = 1 + floor((idx-1)/d); # div
    cs(2:v)= unindex(ds(2:v),i);
  endif
endfunction

#---------------------------------------------------------------------
# assume specific idx2name & idx2rng and global dim and b

function conf = idx2conf (idx)
  global dim;
  global b;

  v = length(dim);           # no of variables
  cs = unindex([dim,b],idx); # (abstract) values
  l = cs(v+1);               # block label

  conf = ["<"];   # --- start configuration

  for i=1:v
    rng = idx2rng(i);              # variable range
    name = idx2name(i);            # variable name
    lbr = find(!(name-"[")); 
    rbr = find(!(name-"]"));   
    if lbr # an array cell
      aname = name(1:lbr-1);             # array name
      asize = arraysize(aname);          # array size
      astart = name2idx([aname,"[1]"]);  # array start
      astop = astart+asize-1;            # array stop
      if (i==astart)
	conf = [conf,aname,"=["];
	conf = [conf,int2str(rng(cs(i))),","];
      elseif (i==astop)
	conf = [conf,int2str(rng(cs(i))),"],"];
      else
	conf = [conf,int2str(rng(cs(i))),","];
      endif
    else # a variable
      conf = [conf,name,"="];
      conf = [conf,int2str(rng(cs(i))),","];
    endif
  endfor

  conf = [conf,"[",lbl2block(l),"]^",int2str(l)]; # block

  conf = [conf,">"]; # --- close configuration

endfunction

#---------------------------------------------------------------------
#---------------------------------------------------------------------
