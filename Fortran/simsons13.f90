program simpson_13
implicit none
integer::i,n
real::a,b,h,s,f,x
f(x)=1/(1+x**2)
read*,n
a=0
b=1
h=(b-a)/n
s=f(a)+f(b)

do i=1,n-1
x=a+(i*h)

if(i/2*2.ne.i)then
s=s+4*f(x)
else
s=s+2*f(x)
end if
end do

s=s*h/3.0
print*,s
end program simpson_13
