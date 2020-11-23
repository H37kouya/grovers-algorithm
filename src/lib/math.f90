module math
    implicit none
    private

    public :: pi, rand

contains
    ! 円周率を返却する
    function pi() result(ret)
        implicit none
        real(4) :: ret
        ret = acos(-1.0)

        return
    end function pi

    function rand(n) result(ret)
        integer :: seedsize, i, n
        double precision, dimension(n) :: ret
        integer, allocatable :: seed(:)

        call random_seed(size=seedsize) !初期値のサイズを取得
        allocate(seed(seedsize)) !配列の割り当て
        do i = 1, seedsize
            call system_clock(count=seed(i)) !時間を取得
        end do
        call random_seed(put=seed(:)) !初期値を与える

        call random_number(ret)
        return
    end function rand
end module math
