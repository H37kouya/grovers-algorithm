program main
    use array
    use math
    use groverQuantumSearchUsecase
    implicit none

    double precision :: groverQuantumSearchAbs
    complex(kind(0d0)), dimension(2 ** 8) :: cArr, groverQuantumSearch, groverQuantumSearchBefore
    integer :: groverQuantumSearchLoop = 2 ** 8 * 2
    complex(kind(0d0)), dimension( 2 ** 8 * 2) :: groverQuantumSearchOnlyTarget, groverQuantumSearchNoTarget
    complex(kind(0d0)), dimension( 2 ** 8 * 2) :: groverQuantumSearchNoMaxTarget, groverQuantumSearchNoMinTarget
    character(8) :: date
    character(10) :: time
    character(128) :: path, tmpChar1, tmpChar2, tmpChar3, tmpChar4, tmpChar5
    integer :: i, j, groverQuantumSearchTarget, maxIdx, minIdx
    ! ファイル書き込み用に現在時刻の取得
    call date_and_time(date, time)
    path = '/mnt/c/Users/kouya/code/fortran-cmake/outputs/'
    cArr = array_normalization(size(cArr))
    maxIdx = array_max_index(cArr)
    minIdx = array_min_index(cArr)

    ! 正規化された値と計算前の複素数のファイルへの書き込み.
    open(18, file=trim(path)//date//'_'//time//'_complex'//'.csv', status='new')
    write(18, *) 'no,probability,probability(%),real,aimag'
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
        tmpChar2 = trim(tmpChar4)//','//trim(tmpChar5)
        write(18, *) trim(adjustl(tmpChar1))//','//trim(adjustl(tmpChar2))
    end do
    close(18)

    groverQuantumSearchBefore = cArr
    groverQuantumSearchTarget = 1
    do i = 1, groverQuantumSearchLoop
        groverQuantumSearch = groverQuantumSearchOnce(groverQuantumSearchBefore, groverQuantumSearchTarget)
        write (tmpChar1,*) i
        tmpChar1 = adjustl(tmpChar1)
!        open(18, file=trim(path)//date//'_'//time//'_result_'//trim(tmpChar1)//'.csv', status='new')
!        write(18, *) 'no,real,aimag,complex,target'
!
!        do j = 1, size(groverQuantumSearch)
!            write (tmpChar2,*) j
!            write (tmpChar3,*) real(groverQuantumSearch(j))
!            write (tmpChar4,*) aimag(groverQuantumSearch(j))
!            tmpChar2 = adjustl(tmpChar2)
!            tmpChar3 = adjustl(tmpChar3)
!            tmpChar4 = adjustl(tmpChar4)
!            if (j == groverQuantumSearchTarget) then
!                tmpChar1 = 'true'
!            else
!                tmpChar1 = 'false'
!            end if
!            tmpChar5 = trim(tmpChar3)//','//trim(tmpChar4)//','//trim(tmpChar3)//'+i'//trim(tmpChar4)
!            write(18, *) trim(adjustl(tmpChar2))//','//trim(adjustl(tmpChar5))//','//trim(adjustl(tmpChar1))
!        end do
!        close(18)

        groverQuantumSearchOnlyTarget(i) = groverQuantumSearch(groverQuantumSearchTarget)
        groverQuantumSearchNoTarget(i) = groverQuantumSearch(groverQuantumSearchTarget + 1)
        groverQuantumSearchNoMaxTarget(i) = groverQuantumSearch(maxIdx)
        groverQuantumSearchNoMinTarget(i) = groverQuantumSearch(minIdx)

        groverQuantumSearchBefore = groverQuantumSearch
    end do


    open(18, file=trim(path)//date//'_'//time//'_result_target.csv', status='new')
    write(18, *) 'no,real,aimag,abs,abs^2'
    do i = 1, size(groverQuantumSearchOnlyTarget)
        write (tmpChar2,*) i
        write (tmpChar3,*) real(groverQuantumSearchOnlyTarget(i))
        write (tmpChar4,*) aimag(groverQuantumSearchOnlyTarget(i))
        tmpChar2 = adjustl(tmpChar2)
        tmpChar3 = adjustl(tmpChar3)
        tmpChar4 = adjustl(tmpChar4)
        tmpChar5 = trim(tmpChar3)//','//trim(tmpChar4)
        groverQuantumSearchAbs = abs(groverQuantumSearchOnlyTarget(i))
        write (tmpChar1,*) groverQuantumSearchAbs
        write (tmpChar3,*) groverQuantumSearchAbs ** 2
        tmpChar1 = trim(adjustl(tmpChar2))//','//trim(adjustl(tmpChar5))//','//trim(adjustl(tmpChar1))
        write(18, *) trim(adjustl(tmpChar1))//','//trim(adjustl(tmpChar3))
    end do
    close(18)

    open(18, file=trim(path)//date//'_'//time//'_result_target_no1.csv', status='new')
    write(18, *) 'no,real,aimag,abs,abs^2'
    do i = 1, size(groverQuantumSearchNoTarget)
        write (tmpChar2,*) i
        write (tmpChar3,*) real(groverQuantumSearchNoTarget(i))
        write (tmpChar4,*) aimag(groverQuantumSearchNoTarget(i))
        tmpChar2 = adjustl(tmpChar2)
        tmpChar3 = adjustl(tmpChar3)
        tmpChar4 = adjustl(tmpChar4)
        tmpChar5 = trim(tmpChar3)//','//trim(tmpChar4)
        groverQuantumSearchAbs = abs(groverQuantumSearchNoTarget(i))
        write (tmpChar1,*) groverQuantumSearchAbs
        write (tmpChar3,*) groverQuantumSearchAbs ** 2
        tmpChar1 = trim(adjustl(tmpChar2))//','//trim(adjustl(tmpChar5))//','//trim(adjustl(tmpChar1))
        write(18, *) trim(adjustl(tmpChar1))//','//trim(adjustl(tmpChar3))
    end do
    close(18)

    open(18, file=trim(path)//date//'_'//time//'_result_target_no1_max.csv', status='new')
    write(18, *) 'no,real,aimag,abs,abs^2'
    do i = 1, size(groverQuantumSearchNoMaxTarget)
        write (tmpChar2,*) i
        write (tmpChar3,*) real(groverQuantumSearchNoMaxTarget(i))
        write (tmpChar4,*) aimag(groverQuantumSearchNoMaxTarget(i))
        tmpChar2 = adjustl(tmpChar2)
        tmpChar3 = adjustl(tmpChar3)
        tmpChar4 = adjustl(tmpChar4)
        tmpChar5 = trim(tmpChar3)//','//trim(tmpChar4)
        groverQuantumSearchAbs = abs(groverQuantumSearchNoMaxTarget(i))
        write (tmpChar1,*) groverQuantumSearchAbs
        write (tmpChar3,*) groverQuantumSearchAbs ** 2
        tmpChar1 = trim(adjustl(tmpChar2))//','//trim(adjustl(tmpChar5))//','//trim(adjustl(tmpChar1))
        write(18, *) trim(adjustl(tmpChar1))//','//trim(adjustl(tmpChar3))
    end do
    close(18)

    open(18, file=trim(path)//date//'_'//time//'_result_target_no1_min.csv', status='new')
    write(18, *) 'no,real,aimag,abs,abs^2'
    do i = 1, size(groverQuantumSearchNoMinTarget)
        write (tmpChar2,*) i
        write (tmpChar3,*) real(groverQuantumSearchNoMinTarget(i))
        write (tmpChar4,*) aimag(groverQuantumSearchNoMinTarget(i))
        tmpChar2 = adjustl(tmpChar2)
        tmpChar3 = adjustl(tmpChar3)
        tmpChar4 = adjustl(tmpChar4)
        tmpChar5 = trim(tmpChar3)//','//trim(tmpChar4)
        groverQuantumSearchAbs = abs(groverQuantumSearchNoMinTarget(i))
        write (tmpChar1,*) groverQuantumSearchAbs
        write (tmpChar3,*) groverQuantumSearchAbs ** 2
        tmpChar1 = trim(adjustl(tmpChar2))//','//trim(adjustl(tmpChar5))//','//trim(adjustl(tmpChar1))
        write(18, *) trim(adjustl(tmpChar1))//','//trim(adjustl(tmpChar3))
    end do
    close(18)
end program main
