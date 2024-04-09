program mpi_spawn_demo
    use mpi
    implicit none

    integer :: ierr, parent_rank, parent_size, child_rank, child_size, row_rank, row_size
    integer :: universe_size  
    integer, parameter :: ensemble_size=3
    logical :: flag, root
    integer :: parentcomm, intercomm, rowcomm
!   character(len=25), allocatable :: commands(:)
!   character(len=25), allocatable :: array_of_argv(:, :)
!   character(len=25), allocatable :: process_dir(:)
!   integer, allocatable :: maxprocs(:)
!   integer, allocatable :: info(:)
!   integer, allocatable :: errcodes(:)
    
    character(len=25) :: commands(ensemble_size)
    character(len=25) :: array_of_argv(ensemble_size, 3)
    character(len=25) :: process_dir(ensemble_size)
    integer :: maxprocs(ensemble_size)
    integer :: info(ensemble_size)
    integer :: errcodes(ensemble_size)

    integer :: color
    integer :: args_maxprocs

    character(len=100) :: input_string
    integer :: iargs, total_signals, signal

    integer :: i, j, k, n, nsize=1000000

    iargs = command_argument_count()
     if (iargs > 0) then
        do i = 1, iargs
            call get_command_argument(i, input_string)
            print *, 'Command line arguments: ',trim(input_string)
            read(input_string,*) args_maxprocs
        end do
    else
        print *, 'No command line arguments provided.'
        args_maxprocs = 96
    end if

    ! Initialize MPI
    ierr = MPI_SUCCESS
    call MPI_Init(ierr)
    call MPI_Comm_rank(MPI_COMM_WORLD, parent_rank, ierr)
    call MPI_Comm_size(MPI_COMM_WORLD, parent_size, ierr)
 
    root=0

    if (parent_size < ensemble_size ) then
      call error_handle(MPI_COMM_WORLD, 1, "parent_size greater than or equal to 3 !! ")
    endif

    call MPI_Attr_Get(MPI_COMM_WORLD, MPI_UNIVERSE_SIZE, universe_size, flag, ierr )
    if (.not. flag) then
      print*,'This MPI does not support UNIVERSE_SIZE. How many processes total?',universe_size
    endif
    if (universe_size == 1 ) then
      call error_handle(MPI_COMM_WORLD, 1, "No room to start workers ")
    else
      print*,'parent: UNIVERSE_SIZE: ',universe_size
    endif

    ! split group for each ensemble member
    color = mod(parent_rank,ensemble_size)
    print*,'parent: color ', color, ' parent_rank', parent_rank, ' ensemble_size',ensemble_size
    call MPI_Comm_split(MPI_COMM_WORLD, color, parent_rank, rowcomm, ierr) 
    call error_handle(MPI_COMM_WORLD, ierr, "MPI_Comm_split")
    print*,'parent: MPI_Comm_split'

    call MPI_Comm_rank(rowcomm, row_rank, ierr)
    call MPI_Comm_size(rowcomm, row_size, ierr)
    ! Print the rank and size of the spawned processes
    print *, "parent: Parent process ", parent_rank, " of ", parent_size, " Row process ", row_rank, " of ", row_size

    ! Set up commands and arguments to be spawned
    !print*,'parent: allocate: rank:', parent_rank
    !allocate(commands(ensemble_size),array_of_argv(ensemble_size, 3), maxprocs(ensemble_size))
    !allocate(info(ensemble_size), errcodes(ensemble_size),process_dir(ensemble_size))

    do i = 1, ensemble_size 
      commands(i) = "./test.exe"
      !array_of_argv(1) = "-gridfile ocean1.grd"  ! First argument is common for both
      write(array_of_argv(i, 1),"(A5,I1)") "./mem",i 
      array_of_argv(i, 2) = ""
      array_of_argv(i, 3) = ""
      write(process_dir(i),"(A5,I1)") "./mem",i
      maxprocs(i) =  args_maxprocs  ! Spawn max process for each command

      ! create info 
      call MPI_Info_Create(info(i), ierr)
      call error_handle(MPI_COMM_WORLD, ierr, "MPI_Info_Create")
      
      ! set info
      CALL MPI_Info_Set(info(i), "wdir", process_dir(i), ierr)
      CALL MPI_INFO_SET(info(i), "num_nodes", "2", ierr)
      CALL MPI_INFO_SET(info(i), "vcoordfile", trim(process_dir(i))//"/vcoordfile", ierr)
      call error_handle(MPI_COMM_WORLD, ierr, "MPI_Info_Set")
    enddo

    !call MPI_Barrier(rowcomm, ierr)
    !print*,'MPI_Barrier(rowcomm, ierr)'
!    if (row_rank == 0 ) then
    ! Spawn processee
    call MPI_Comm_spawn(commands(parent_rank+1), array_of_argv(parent_rank+1,:), &
                        maxprocs(parent_rank+1), info(parent_rank+1), 0, rowcomm, intercomm, errcodes, ierr)
    call error_handle(rowcomm, ierr, "MPI_Comm_spawn_multiple")
    print*,'parent: MPI_Comm_spawn'


    if (row_rank == 0) then
      ! Parent process waits for all child processes to finish
      total_signals = maxprocs(1)
      print*,'parent: MPI_Recv: parent_rank: ',parent_rank
      do i = 1, maxprocs(1)
            print*,'parent: parent_rank: ',parent_rank,' child rank: ',i,' reciving signal ...'
            call MPI_Recv(signal, 1, MPI_INTEGER, i-1, MPI_ANY_TAG, intercomm, MPI_STATUS_IGNORE, ierr)
            print*,'parent: parent_rank: ',parent_rank,' child rank: ',i,' recived signal: ',signal
            total_signals = total_signals - 1
      enddo
    endif
   print*,'parent: total_signals: ',total_signals

   call MPI_Barrier(intercomm, ierr)
   call error_handle(intercomm, ierr, "MPI_Barrier(intercomm, ierr)")
   print*,'parent: MPI_Barrier(intercomm, ierr): parent_rank: ', parent_rank

   print*,'parent: MPI_Barrier(rowcomm, ierr): parent_rank: ', parent_rank
   call MPI_Barrier(rowcomm, ierr)

   print*,'parent: MPI_Barrier(MPI_COMM_WORLD, ierr): parent_rank: ', parent_rank
   call MPI_Barrier(MPI_COMM_WORLD, ierr)

    !print*,'parent: MPI_Info_free'
    !do i = 1, ensemble_size 
    !  call MPI_Info_free(info(i), ierr)
    !enddo
    !deallocate(commands, array_of_argv, maxprocs, info, errcodes, process_dir)

    call MPI_Comm_free(rowcomm, ierr)
    print*,'parent: MPI_Comm_free(rowcomm, ierr): parent_rank: ', parent_rank

   !print*,'parent: deallocate: parent_rank: ', parent_rank
   !deallocate(commands, array_of_argv, maxprocs, info, errcodes, process_dir)
   !call system("sleep 10")
   
    ! End MPI
   print*,'parent: MPI_Barrier(MPI_COMM_WORLD, ierr): parent_rank: ', parent_rank
   call MPI_Barrier(MPI_COMM_WORLD, ierr)

   print*,'MPI_Finalize(ierr): parent_rank: ', parent_rank
   call MPI_Finalize(ierr)
    
end program mpi_spawn_demo

subroutine error_handle(comm, ierr, cname)
  use mpi
  implicit none
  integer :: ierr, comm
  character(len=*) :: cname

  if (ierr /= MPI_SUCCESS) then
       print *, "Error for ",cname
       call MPI_Abort(comm, 1, ierr)
!  else
!      print *, "Success for ",cname
  endif

end subroutine error_handle
