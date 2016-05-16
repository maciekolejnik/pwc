begin
init: x=0, a[0]=0, a[1]=0, y=0
step 1: x=0, a[0]=0, a[1]=0, y=0; block=2
step 2: x=0, a[0]=1/2:0 or 1/2:1, a[1]=0, y=0; block=3
step 50: x=0, a[0]=1/2:0 or 1/2:1, a[1]=0, y=0; block=3
end

begin
init: x=0, a[0]=0, a[1]=0, y=1
step 1: x=0, a[0]=1, a[1]=0, y=1; block=2
step 2: x=0, a[0]=1, a[1]=1/2:0 or 1/2:1, y=1; block=3
step 50: x=0, a[0]=1, a[1]=1/2:0 or 1/2:1, y=1; block=3
end

begin
init: x=1, a[0]=0, a[1]=0, y=1
step 1: x=1, a[0]=0, a[1]=1, y=1; block=2
step 2: x=1, a[0]=0, a[1]=1/2:0 or 1/2:1, y=1; block=3
step 50: x=1, a[0]=0, a[1]=1/2:0 or 1/2:1, y=1; block=3
end

begin
init: x=1/2:0 or 1/2:1, a[0]=0, a[1]=0, y=1
step 1:  1/2: x=0, a[0]=1, a[1]=0, y=1 ; 1/2: x=1, a[0]=0, a[1]=1, y=1; block=2
step 2:  1/2: x=0, a[0]=1, a[1]=1/2:0 or 1/2:1, y=1 ;
         1/2: x=1, a[0]=0, a[1]=1/2:0 or 1/2:1, y=1; block=3
step 50: 1/2: x=0, a[0]=1, a[1]=1/2:0 or 1/2:1, y=1 ; 
         1/2: x=1, a[0]=0, a[1]=1/2:0 or 1/2:1, y=1; block=3
end
