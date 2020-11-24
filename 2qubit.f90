program main
    use array
    use math
    use groverQuantumSearchUsecase
    implicit none

    double precision :: groverQuantumSearchAbs
    complex(kind(0d0)), dimension(2 ** 2) :: cArr, groverQuantumSearch, groverQuantumSearchBefore
    integer :: groverQuantumSearchLoop = 12
    complex(kind(0d0)), dimension(12) :: groverQuantumSearchOnlyTarget
    complex(kind(0d0)), dimension(12) :: groverQuantumSearchNoMaxTarget, groverQuantumSearchNoMinTarget
    character(8) :: date
    character(10) :: time
    character(128) :: path, tmpChar1, tmpChar2, tmpChar3, tmpChar4, tmpChar5
    integer :: i, j, groverQuantumSearchTarget, maxIdx, minIdx
    ! ファイル書き込み用に現在時刻の取得
    call date_and_time(date, time)
    path = '/mnt/c/Users/kouya/code/fortran-cmake/outputs/'
    cArr(1) = complex(0.5d0, 0)
    cArr(2) = complex(0.5d0, 0)
    cArr(3) = complex(0.5d0, 0)
    cArr(4) = complex(0.5d0, 0)

    ! 正規化された値と計算前の複素数のファイルへの書き込み.
    open(18, file=trim(path)//date//'_'//time//'_complex'//'.csv', status='new')
    write(18, *) 'no,probability,probability(%),real,aimag,complex'
    do i = 1, size(cArr)
        write (tmpChar1,*) i
        write (tmpChar2,*) abs(cArr(i)) ** 2
        write (tmpChar3,*) abs(cArr(i)) ** 2 * 100
        write (tmpChar4,*) real(cArr(i))
        write (tmpChar5,*) aimag(cArr(i))
        tmpChar1 = adjustl(tmpChar1)
        tmpChar2 = adjustl(tmpChar2)
        tmpChar3 = adjustl(tmpChar3)
        tmpChar4 = adjustl(tmpChar4)
        tmpChar5 = adjustl(tmpChar5)
        tmpChar1 = trim(tmpChar1)//','//trim(tmpChar2)//','//trim(tmpChar3)
        tmpChar2 = trim(tmpChar4)//','//trim(tmpChar5)//','//trim(tmpChar4)//'+i'//trim(tmpChar5)
        write(18, *) trim(adjustl(tmpChar1))//','//trim(adjustl(tmpChar2))
    end do
    close(18)

    groverQuantumSearchBefore = cArr
    groverQuantumSearchTarget = 1
    do i = 1, groverQuantumSearchLoop
        groverQuantumSearch = groverQuantumSearchOnce(groverQuantumSearchBefore, groverQuantumSearchTarget)
        write (tmpChar1,*) i
        tmpChar1 = adjustl(tmpChar1)
        open(18, file=trim(path)//date//'_'//time//'_result_'//trim(tmpChar1)//'.csv', status='new')
        write(18, *) 'no,real,aimag,complex,target'

        do j = 1, size(groverQuantumSearch)
            write (tmpChar2,*) j
            write (tmpChar3,*) real(groverQuantumSearch(j))
            write (tmpChar4,*) aimag(groverQuantumSearch(j))
            tmpChar2 = adjustl(tmpChar2)
            tmpChar3 = adjustl(tmpChar3)
            tmpChar4 = adjustl(tmpChar4)
            if (j == groverQuantumSearchTarget) then
                tmpChar1 = 'true'
            else
                tmpChar1 = 'false'
            end if
            tmpChar5 = trim(tmpChar3)//','//trim(tmpChar4)//','//trim(tmpChar3)//'+i'//trim(tmpChar4)
            write(18, *) trim(adjustl(tmpChar2))//','//trim(adjustl(tmpChar5))//','//trim(adjustl(tmpChar1))
        end do
        close(18)

        groverQuantumSearchOnlyTarget(i) = groverQuantumSearch(groverQuantumSearchTarget)

        groverQuantumSearchBefore = groverQuantumSearch
    end do


    open(18, file=trim(path)//date//'_'//time//'_result_target.csv', status='new')
    write(18, *) 'no,real,aimag,complex,abs,abs^2'
    do i = 1, size(groverQuantumSearchOnlyTarget)
        write (tmpChar2,*) i
        write (tmpChar3,*) real(groverQuantumSearchOnlyTarget(i))
        write (tmpChar4,*) aimag(groverQuantumSearchOnlyTarget(i))
        tmpChar2 = adjustl(tmpChar2)
        tmpChar3 = adjustl(tmpChar3)
        tmpChar4 = adjustl(tmpChar4)
        tmpChar5 = trim(tmpChar3)//','//trim(tmpChar4)//','//trim(tmpChar3)//'+i'//trim(tmpChar4)
        groverQuantumSearchAbs = abs(groverQuantumSearchOnlyTarget(i))
        write (tmpChar1,*) groverQuantumSearchAbs
        write (tmpChar3,*) groverQuantumSearchAbs ** 2
        tmpChar1 = trim(adjustl(tmpChar2))//','//trim(adjustl(tmpChar5))//','//trim(adjustl(tmpChar1))
        write(18, *) trim(adjustl(tmpChar1))//','//trim(adjustl(tmpChar3))
    end do
    close(18)
end program main
