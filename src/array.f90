module array
    implicit none
    private

    public :: array_sum_complex, array_square_sum_complex, array_sign_inversion

contains
    ! 複素数配列の合計値を求める
    function array_sum_complex(arr, count) result(ret)
        complex(kind(0d0)), dimension(:) :: arr
        real(8) :: ret
        real(8) total
        integer count, i

        total = 0
        do i = 1, count
            total = total + arr(i)
        end do
        ret = total
        return
    end function array_sum_complex

    ! 複素数配列の2乗の合計値を求める
    function array_square_sum_complex(arr, count) result(ret)
        complex(kind(0d0)), dimension(:) :: arr
        real(8) :: ret
        real(8) total
        integer count, i

        total = 0
        do i = 1, count
            total = total + arr(i) ** 2
        end do
        ret = total
        return
    end function array_square_sum_complex

    ! 配列の符号を反転させる。targetIndexesで判定したい符号を指定する。
    function array_sign_inversion(arr, targetIndexs) result(ret)
        complex(kind(0d0)), dimension(:) :: arr
        complex(kind(0d0)), dimension(size(arr)) :: ret
        integer, dimension(:) :: targetIndexs
        integer i

        ret = arr
        do i = 1, size(targetIndexs)
            ret(targetIndexs(i)) = -1 * ret(targetIndexs(i))
        end do
        return
    end function array_sign_inversion
end module array