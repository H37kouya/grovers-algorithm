module array
    use math
    implicit none
    private

    public :: array_sum_complex, array_square_sum_complex, array_sign_inversion
    public :: array_sign_inversion_to_all_value, array_add_complex_to_each_value, array_normalization
    public :: array_max_index, array_min_index
contains
    ! 複素数配列の合計値を求める
    function array_sum_complex(arr) result(ret)
        complex(kind(0d0)), dimension(:) :: arr
        complex(kind(0d0)) :: ret
        integer i

        ret = 0
        do i = 1, size(arr)
            ret = ret + arr(i)
        end do
        return
    end function array_sum_complex

    ! 複素数配列の2乗の合計値を求める
    function array_square_sum_complex(arr, count) result(ret)
        complex(kind(0d0)), dimension(:) :: arr
        complex(kind(0d0)) :: ret
        integer count, i

        ret = 0
        do i = 1, count
            ret = ret + arr(i) ** 2
        end do
        return
    end function array_square_sum_complex

    ! 配列の符号を反転させる。targetIndexesで判定したい符号を指定する
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

    ! 全ての配列の符号を反転させる
    function array_sign_inversion_to_all_value(arr) result(ret)
        complex(kind(0d0)), dimension(:) :: arr
        complex(kind(0d0)), dimension(size(arr)) :: ret
        integer i

        ret = arr
        do i = 1, size(arr)
            ret(i) = -1 * ret(i)
        end do
        return
    end function array_sign_inversion_to_all_value

    ! 全ての配列に対して、ある数を足す
    function array_add_complex_to_each_value(arr, addComplex) result(ret)
        complex(kind(0d0)), dimension(:) :: arr
        complex(kind(0d0)), dimension(size(arr)) :: ret
        complex(kind(0d0)) :: addComplex
        integer i

        ret = arr
        do i = 1, size(arr)
            ret(i) = ret(i) + addComplex
        end do
        return
    end function array_add_complex_to_each_value

    ! 正規化された配列を生成する
    function array_normalization(quantity) result(ret)
        integer :: quantity
        double precision, dimension(quantity * 2) :: randArr
        complex(kind(0d0)), dimension(quantity) :: ret
        double precision total
        integer i

        total = 0
        randArr = rand(quantity * 2)
        do i = 1, quantity
            ret(i) = complex(randArr(2*i - 1), randArr(2*i))
            total = total + abs(ret(i)) ** 2
        end do

        total = sqrt(total)
        do i = 1, quantity
            ret(i) = complex(real(ret(i)) / total, aimag(ret(i)) / total)
        end do
        return
    end function array_normalization

    ! 配列の中から最大の値のindexを取得する
    function array_max_index(arr) result(index)
        complex(kind(0d0)), dimension(:) :: arr
        double precision tmpVal
        integer :: i
        integer :: index

        tmpVal = abs(arr(1))
        do i = 2, size(arr)
            if (tmpVal < abs(arr(i))) then
                tmpVal = abs(arr(i))
                index = i
            end if
        end do

    end function array_max_index

    ! 配列の中から最小の値のindexを取得する
    function array_min_index(arr) result(index)
        complex(kind(0d0)), dimension(:) :: arr
        double precision tmpVal
        integer :: i
        integer :: index

        tmpVal = abs(arr(1))
        do i = 2, size(arr)
            if (tmpVal > abs(arr(i))) then
                tmpVal = abs(arr(i))
                index = i
            end if
        end do
    end function array_min_index
end module array
