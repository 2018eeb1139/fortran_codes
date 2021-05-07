program simpson_38
implicit none
integer::i,n
real::a,b,h,s,f,x
f(x)=1/(1+x**2)
read*,n
a=0
b=1
h=(b-a)/n
s=f(a)+f(b)

do i=1,n
x=a+(i*h)

if((i/3)*3==i)then
s=s+2*f(x)
else
s=s+3*f(x)
end if
end do

s=s*(3*h/8)
print*,s
end program simpson_38
