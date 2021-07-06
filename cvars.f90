!----------------------------------------------------------
! Generate a sequence of correlated Bernoulli variables   
! author: Matthew Kukla, <matt.kukla@yandex.ru>           
!----------------------------------------------------------

module cvars

    implicit none 
    
    public 

    contains

    function corbernseq(n, p, delta) result(seq)
        implicit none
        integer :: n, k 
        double precision :: j,p, delta, z, u, pp, pm, qp, qm
        double precision, dimension(n) :: seq
        intent (in) n, p, delta
        z = 1-p
        call random_number(u)

        pp =  p + delta*(1-p)
        pm = p - delta*(p)
        qp = 1 - p + delta*p
        qm = 1 - p - delta*(1-p)
        seq = 1
        if (z <= u) then
            seq(1) = 1
            j = p * qm
            z = z + j
        else
            seq(1) = 0
            j = (1-p) * pm
            z = z - j
        end if
        k = 2
        do while (k <= n)
            if (z <= u) then
                seq(k) = 1
                j = j * pp
                z = z + j
            else
                seq(k) = 0
                j = j * pp
                z = z - j
            end if
            k = k + 1
        end do

    end function corbernseq

end module cvars
