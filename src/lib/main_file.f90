module main_file
    implicit none
    private

    public :: grover_quantum_search_in_file

contains
    function grover_quantum_search_in_file(arr, filePath) result(ret)
        complex(kind(0d0)), dimension(:) :: arr
        character(128) :: filePath
        character(128) :: idxStr, realStr, aimagStr, absStr, abs2Str, tmpStr1, tmpStr2
        double precision :: groverQuantumSearchAbs
        integer i, ret
        ret = 0

        open(18, file=trim(adjustl(filePath)), status='new')
        write(18, *) 'no,real,aimag,abs,abs^2'
        do i = 1, size(arr)
            ! index の文字列化
            write (idxStr,*) i
            idxStr = trim(adjustl(idxStr))

            ! 実部の文字列化
            write (realStr,*) real(arr(i))
            realStr = trim(adjustl(realStr))

            ! 虚部の文字列化
            write (aimagStr,*) aimag(arr(i))
            aimagStr = trim(adjustl(aimagStr))

            groverQuantumSearchAbs = abs(arr(i))
            write (absStr,*) groverQuantumSearchAbs
            write (abs2Str,*) groverQuantumSearchAbs ** 2
            absStr = trim(adjustl(absStr))
            abs2Str = trim(adjustl(abs2Str))

            tmpStr1 = idxStr//','//realStr//','//aimagStr
            tmpStr2 = absStr//','//abs2Str
            write(18, *) trim(adjustl(tmpStr1))//','//trim(adjustl(tmpStr2))
        end do
        close(18)
        return
    end function grover_quantum_search_in_file
end module main_file