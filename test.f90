program main

  use mpi
  implicit none

  integer :: nprocs, myrank, ierr
  integer :: i, j, k, n, tot, nsize=100
  integer :: parentcomm, parent_rank, parent_nprocs

  character(len=256) :: command_line
  integer :: signal
  integer :: iargs
  character(len=256) :: input_string
  character(len=256) :: prefix

  call MPI_INIT(ierr)
  call MPI_COMM_GET_PARENT(parentcomm, ierr)
  call MPI_COMM_SIZE(MPI_COMM_WORLD, nprocs, ierr)
  call MPI_COMM_RANK(MPI_COMM_WORLD, myrank, ierr)
 
  if (parentcomm == MPI_COMM_NULL) then 
    prefix='Alone:'
  else
    prefix='Child:'
  endif
 
  iargs = command_argument_count()
   if (iargs > 0) then
      do i = 1, iargs
          call get_command_argument(i, input_string)
            print *, trim(prefix), ' Command line arguments: ',trim(input_string)
      end do
  else
      print *, trim(prefix), ' No command line arguments provided.'
      input_string = 'None'
  end if

  write(prefix,'(A6,1X,A7,A1,1X)')trim(prefix),trim(input_string),':'

  if (parentcomm == MPI_COMM_NULL) then 
    print *, trim(prefix), ' Hello from child rank ', myrank, ' of ', nprocs
  else
    parent_rank = 0
    call MPI_COMM_REMOTE_SIZE(parentcomm, parent_nprocs, ierr)
    print *, trim(prefix), ' Hello from child rank ', myrank, ' of ', nprocs, ', parent rank ',parent_rank, ' of ', parent_nprocs
    call MPI_Barrier(MPI_COMM_WORLD, ierr)
  endif

  n = myrank + 1

  call MPI_REDUCE(n, tot, 1, MPI_INTEGER, MPI_SUM, 0, MPI_COMM_WORLD, ierr)

  if (myrank == 0) print *, trim(prefix),' Total = ', tot

  if (parentcomm /= MPI_COMM_NULL) then 
    signal = n
    call MPI_Send(signal, 1, MPI_INTEGER, 0, 0, parentcomm, ierr)

    call MPI_Barrier(parentcomm, ierr)
  endif

  !call MPI_Barrier(MPI_COMM_WORLD, ierr)
  call MPI_FINALIZE(ierr)

end program main
