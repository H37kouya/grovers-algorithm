program main
    use array
    use math
    implicit none

    complex(kind(0d0)), dimension(4) :: cArr
    complex(kind(0d0)) c1, c2, c3, c4
    doubleprecision x, abs_c
    x = 1 / 2.0
    cArr(1) = x + (0, 0d0)
    cArr(2) = x + (0, 0d0)
    cArr(3) = x + (0, 0d0)
    cArr(4) = x + (0, 0d0)

    abs_c = array_square_sum_complex(cArr, size(cArr))

    write(*,*) abs_c
    write(*,*) array_sign_inversion(cArr, (/ 2, 3 /))
end program main
