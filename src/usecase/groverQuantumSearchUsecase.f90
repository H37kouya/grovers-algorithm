module groverQuantumSearchUsecase
    use array
    implicit none

    private

    public :: groverQuantumSearchOnce
contains

    function groverQuantumSearchOnce(arr, target) result(output)
        complex(kind(0d0)), dimension(:) :: arr
        complex(kind(0d0)), dimension(size(arr)) :: output
        complex(kind(0d0)), dimension(size(arr)) :: arrDoSignInversion, arrAllDoSignInversion
        complex(kind(0d0)) :: averageDoSingInversion
        integer, dimension(:) :: target

        !1 オラクルUf いわゆる、γ
        arrDoSignInversion = array_sign_inversion(arr, (/ target /))

        !2-1 P に γ を入れる
        averageDoSingInversion = array_sum_complex(arrDoSignInversion) / size(arrDoSignInversion)
        !2-2 配列の符号をすべて反転させる
        arrAllDoSignInversion = array_sign_inversion_to_all_value(arrDoSignInversion)
        !2-3 全ての配列に対して、平均を操作する
        output = array_add_complex_to_each_value(arrAllDoSignInversion, 2 * averageDoSingInversion)

        return
    end function groverQuantumSearchOnce
end module groverQuantumSearchUsecase