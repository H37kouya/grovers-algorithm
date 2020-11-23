module array
    use math
    implicit none
    private

    public :: array_sum_complex, array_square_sum_complex, array_sign_inversion
    public :: array_sign_inversion_to_all_value, array_add_complex_to_each_value, array_normalization
    public :: array_normalization_to_sqrt_complex
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

    function array_normalization(quantity) result(ret)
        integer :: quantity
        double precision, dimension(quantity) :: ret
        double precision total
        integer i

        total = 0
        ret = rand(quantity)
        do i = 1, quantity
            total = total + ret(i)
        end do

        do i = 1, quantity
            ret(i) = ret(i) / total
        end do
        return
    end function array_normalization

    function array_normalization_to_sqrt_complex(arrNormalization) result(ret)
        double precision, dimension(:) :: arrNormalization
        complex(kind(0d0)), dimension(size(arrNormalization)) :: ret
        double precision :: sqrtNum, realNum
        integer i

        ret = rand(size(arrNormalization))
        do i = 1, size(arrNormalization)
            realNum = ret(i)
            ret(i) = complex(realNum, sqrt(arrNormalization(i) - realNum ** 2))
        end do
    end function array_normalization_to_sqrt_complex

    ! 配列の中から最大の値のindexを取得する
    function array_max_index(arr) result(index)
        double precision, dimension(:) :: arr
        double precision tmpVal
        integer :: i
        integer :: index

        tmpVal = arr(1)
        do i = 2, size(arr)
            if (tmpVal < arr(i)) then
                tmpVal = arr(i)
                index = i
            end if
        end do

    end function array_max_index

    ! 配列の中から最小の値のindexを取得する
    function array_min_index(arr) result(index)
        double precision, dimension(:) :: arr
        double precision tmpVal
        integer :: i
        integer :: index

        tmpVal = arr(1)
        do i = 2, size(arr)
            if (tmpVal > arr(i)) then
                tmpVal = arr(i)
                index = i
            end if
        end do
    end function array_min_index
end module array
