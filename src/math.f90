module math
    implicit none
    private

    public :: pi

contains
    ! 円周率を返却する
    function pi() result(ret)
        implicit none
        real(4) :: ret
        ret = acos(-1.0)

        return
    end function pi
end module math
