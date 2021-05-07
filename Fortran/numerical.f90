program numerical_integration

integer::ch
read*,ch
select case (ch)
	case(1)
		call trapezoidal()
	case(2)
		 call simpson13()
	case(3)
		call simpson38()
	case default
		print*,"A"
end select

end program numerical_integration

subroutine trapezoidal
implicit none
integer::i,n
real::a,b,h,p,integration,s,f
n=8
a=0
b=1
h=(b-a)/n
p=(h/2.)*(call f(a)+call f(b))
s=0
do i=1,n-1
s=s+h*call f(a+i*h)
end do
integration=p+s
print*,"The integrated value using trapezoidal rule is",integration
end subroutine


subroutine simpson_13
implicit none
integer::i,n
real::a,b,h,s,x
n=8
a=0
b=1
h=(b-a)/n
s=call f(a)+call f(b)

do i=1,n-1
x=a+(i*h)

if(i/2*2.ne.i)then
s=s+4*call f(x)
else
s=s+2*call f(x)
end if
end do

s=s*h/3.0
print*,s
end subroutine

subroutine simpson_38
implicit none
integer::i,n
real::a,b,h,s,x
n=8
a=0
b=1
h=(b-a)/n
s=call f(a)+call f(b)

do i=1,n
x=a+(i*h)

if((i/3)*3==i)then
s=s+2*call f(x)
else
s=s+3*call f(x)
end if
end do

s=s*(3*h/8)
print*,s
end subroutine simpson_38

subroutine f(x)
	double precision::x
	1/(1+x**2)
end subroutine f(x)
