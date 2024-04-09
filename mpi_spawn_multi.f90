program mpi_spawn_demo
    use mpi
    implicit none

    integer :: i, ierr, parent_rank, parent_size, child_rank, child_size
    integer :: universe_size  
    integer :: ncommands, root
    logical :: flag
    character(len=25), allocatable :: commands(:)
    character(len=25), allocatable :: array_of_argv(:, :)
    integer, allocatable :: maxprocs(:)
    integer, allocatable :: info(:)
    integer :: parentcomm, intercomm
    integer, allocatable :: errcodes(:)
    character(len=25), allocatable :: process_dir(:)

    ! Initialize MPI
    ierr = MPI_SUCCESS
    call MPI_Init(ierr)
    call MPI_Comm_Get_Parent(parentcomm, ierr)
    call MPI_Comm_rank(MPI_COMM_WORLD, parent_rank, ierr)
    call MPI_Comm_size(MPI_COMM_WORLD, parent_size, ierr)

    if (parent_size /= 1 ) then
      call error_handle(1, "parent_size greate than 1 !! ")
    endif

    call MPI_Attr_Get(MPI_COMM_WORLD, MPI_UNIVERSE_SIZE, universe_size, flag, ierr )
    if (.not. flag) then
      print*,'This MPI does not support UNIVERSE_SIZE. How many processes total?',universe_size
    endif
    if (universe_size == 1 ) then
      call error_handle(1, "No room to start workers ")
    else
      print*,'UNIVERSE_SIZE: ',universe_size
    endif

    if (parentcomm == MPI_COMM_NULL) then
      ! Set up commands and arguments to be spawned
      ncommands = 3
      allocate(commands(ncommands),array_of_argv(ncommands, 3), maxprocs(ncommands))
      allocate(info(ncommands), errcodes(ncommands),process_dir(ncommands))
      root = 0

      do i = 1, ncommands 
        commands(i) = " ../test.exe"
        !array_of_argv(1) = "-gridfile ocean1.grd"  ! First argument is common for both
        array_of_argv(i, 1) = ""  ! First argument is common for both
        array_of_argv(i, 2) = ""
        array_of_argv(i, 3) = ""
        write(process_dir(i),"(A5,I1)") "./mem",i
        maxprocs(i) = 8  ! Spawn one process for each command

        ! create info 
        call MPI_Info_Create(info(i), ierr)
        call error_handle(ierr, "MPI_Info_Create")
        
        ! set info
        CALL MPI_Info_Set(info(i), "wdir", process_dir(i), ierr)
        !CALL MPI_INFO_SET(info(i), "num_nodes", "6", ierr)
        !CALL MPI_INFO_SET(info(i), "vcoordfile", trim(process_dir(i))//"/vcoordfile", ierr)
        call error_handle(ierr, "MPI_Info_Set")
      enddo
 
      
      ! Spawn processes
      call MPI_Comm_spawn_multiple(ncommands, commands, array_of_argv, &
                           maxprocs, info, root, MPI_COMM_WORLD, intercomm, errcodes, ierr)
      call error_handle(ierr, "MPI_Comm_spawn_multiple")
 
      ! Print the rank and size of the spawned processes
      print *, "Parent process ", parent_rank, " of ", parent_size

    else
      print *, "Spqwend process ", child_rank, " of ", child_size
    endif

    ! Disconnect from spawned processes
    call MPI_Barrier(MPI_COMM_WORLD, ierr)

    ! End MPI
    call MPI_Finalize(ierr)
end program mpi_spawn_demo

subroutine error_handle(ierr, cname)
  use mpi
  implicit none
  integer :: ierr
  character(len=128) :: cname

  if (ierr /= MPI_SUCCESS) then
       print *, "Error for ",cname
       call MPI_Abort(MPI_COMM_WORLD, 1, ierr)
  endif

end subroutine error_handle
