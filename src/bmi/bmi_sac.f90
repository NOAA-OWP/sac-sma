module bmi_sac_module

! NGEN_ACTIVE is to be set when running in the Nextgen framework
! https://github.com/NOAA-OWP/ngen
#ifdef NGEN_ACTIVE
   use bmif_2_0_iso
#else
   use bmif_2_0
#endif

  use runModule 
  use, intrinsic :: iso_c_binding, only: c_ptr, c_loc, c_f_pointer
  implicit none

  type, extends (bmi) :: bmi_sac
     private
     type (sac_type) :: model
   contains
     procedure :: get_component_name => sac_component_name
     procedure :: get_input_item_count => sac_input_item_count
     procedure :: get_output_item_count => sac_output_item_count
     procedure :: get_input_var_names => sac_input_var_names
     procedure :: get_output_var_names => sac_output_var_names
     procedure :: initialize => sac_initialize
     procedure :: finalize => sac_finalize
     procedure :: get_start_time => sac_start_time
     procedure :: get_end_time => sac_end_time
     procedure :: get_current_time => sac_current_time
     procedure :: get_time_step => sac_time_step
     procedure :: get_time_units => sac_time_units
     procedure :: update => sac_update
     procedure :: update_until => sac_update_until
     procedure :: get_var_grid => sac_var_grid
     procedure :: get_grid_type => sac_grid_type
     procedure :: get_grid_rank => sac_grid_rank
     procedure :: get_grid_shape => sac_grid_shape
     procedure :: get_grid_size => sac_grid_size
     procedure :: get_grid_spacing => sac_grid_spacing
     procedure :: get_grid_origin => sac_grid_origin
     procedure :: get_grid_x => sac_grid_x
     procedure :: get_grid_y => sac_grid_y
     procedure :: get_grid_z => sac_grid_z
     procedure :: get_grid_node_count => sac_grid_node_count
     procedure :: get_grid_edge_count => sac_grid_edge_count
     procedure :: get_grid_face_count => sac_grid_face_count
     procedure :: get_grid_edge_nodes => sac_grid_edge_nodes
     procedure :: get_grid_face_edges => sac_grid_face_edges
     procedure :: get_grid_face_nodes => sac_grid_face_nodes
     procedure :: get_grid_nodes_per_face => sac_grid_nodes_per_face
     procedure :: get_var_type => sac_var_type
     procedure :: get_var_units => sac_var_units
     procedure :: get_var_itemsize => sac_var_itemsize
     procedure :: get_var_nbytes => sac_var_nbytes
     procedure :: get_var_location => sac_var_location
     procedure :: get_value_int => sac_get_int
     procedure :: get_value_float => sac_get_float
     procedure :: get_value_double => sac_get_double
     generic :: get_value => &
          get_value_int, &
          get_value_float, &
          get_value_double
      procedure :: get_value_ptr_int => sac_get_ptr_int
      procedure :: get_value_ptr_float => sac_get_ptr_float
      procedure :: get_value_ptr_double => sac_get_ptr_double
      generic :: get_value_ptr => &
           get_value_ptr_int, &
           get_value_ptr_float, &
           get_value_ptr_double
      procedure :: get_value_at_indices_int => sac_get_at_indices_int
      procedure :: get_value_at_indices_float => sac_get_at_indices_float
      procedure :: get_value_at_indices_double => sac_get_at_indices_double
      generic :: get_value_at_indices => &
           get_value_at_indices_int, &
           get_value_at_indices_float, &
           get_value_at_indices_double
     procedure :: set_value_int => sac_set_int
     procedure :: set_value_float => sac_set_float
     procedure :: set_value_double => sac_set_double
     generic :: set_value => &
          set_value_int, &
          set_value_float, &
          set_value_double
      procedure :: set_value_at_indices_int => sac_set_at_indices_int
      procedure :: set_value_at_indices_float => sac_set_at_indices_float
      procedure :: set_value_at_indices_double => sac_set_at_indices_double
      generic :: set_value_at_indices => &
           set_value_at_indices_int, &
           set_value_at_indices_float, &
           set_value_at_indices_double
!      procedure :: print_model_info
  end type bmi_sac

  private
  public :: bmi_sac

  character (len=BMI_MAX_COMPONENT_NAME), target :: &
       component_name = "OWP Sac-SMA Module"

  ! Exchange items
  integer, parameter :: input_item_count = 3
  integer, parameter :: output_item_count = 4
  character (len=BMI_MAX_VAR_NAME), target, &
       dimension(input_item_count) :: input_items
  character (len=BMI_MAX_VAR_NAME), target, &
       dimension(output_item_count) :: output_items 

contains

  ! Get the name of the model.
  function sac_component_name(this, name) result (bmi_status)
    class (bmi_sac), intent(in) :: this
    character (len=*), pointer, intent(out) :: name
    integer :: bmi_status

    name => component_name
    bmi_status = BMI_SUCCESS
  end function sac_component_name

  ! Count the input variables.
  function sac_input_item_count(this, count) result (bmi_status)
    class (bmi_sac), intent(in) :: this
    integer, intent(out) :: count
    integer :: bmi_status

    count = input_item_count
    bmi_status = BMI_SUCCESS
  end function sac_input_item_count

  ! Count the output variables.
  function sac_output_item_count(this, count) result (bmi_status)
    class (bmi_sac), intent(in) :: this
    integer, intent(out) :: count
    integer :: bmi_status

    count = output_item_count
    bmi_status = BMI_SUCCESS
  end function sac_output_item_count

  ! List input variables.
  function sac_input_var_names(this, names) result (bmi_status)
    class (bmi_sac), intent(in) :: this
    character (*), pointer, intent(out) :: names(:)
    integer :: bmi_status

    input_items(1) = 'tair'     ! mean air temperature (C)
    input_items(2) = 'precip'   ! total precipitation (mm)
    input_items(3) = 'pet'      ! potential evapotranspiration (mm) 

    names => input_items
    bmi_status = BMI_SUCCESS
  end function sac_input_var_names

  ! List output variables.
  function sac_output_var_names(this, names) result (bmi_status)
    class (bmi_sac), intent(in) :: this
    character (*), pointer, intent(out) :: names(:)
    integer :: bmi_status

    output_items(1) = 'qs'      ! runoff from direct runoff, impervious runoff, surface runoff, and interflow (mm)
    output_items(2) = 'qg'      ! baseflow (mm)
    output_items(3) = 'tci'     ! total channel inflow from upstream (mm)
    output_items(4) = 'eta'     ! actual evapotranspiration (mm) ??

    names => output_items
    bmi_status = BMI_SUCCESS
  end function sac_output_var_names

  ! BMI initializer.
  function sac_initialize(this, config_file) result (bmi_status)
    class (bmi_sac), intent(out) :: this
    character (len=*), intent(in) :: config_file
    integer :: bmi_status

    if (len(config_file) > 0) then
       call initialize_from_file(this%model, config_file)

    !else
       !call initialize_from_defaults(this%model)
     end if

    bmi_status = BMI_SUCCESS
  end function sac_initialize

  ! BMI finalizer.
  function sac_finalize(this) result (bmi_status)
    class (bmi_sac), intent(inout) :: this
    integer :: bmi_status

    call cleanup(this%model)
    bmi_status = BMI_SUCCESS
  end function sac_finalize

  ! Model start time.
  function sac_start_time(this, time) result (bmi_status)
    class (bmi_sac), intent(in) :: this
    double precision, intent(out) :: time
    integer :: bmi_status

    !time = 0.d0                                            ! time relative to start time (s) == 0
    time = dble(this%model%runinfo%start_datetime)         ! using unix time (s)
    
    bmi_status = BMI_SUCCESS
  end function sac_start_time

  ! Model end time.
  function sac_end_time(this, time) result (bmi_status)
    class (bmi_sac), intent(in) :: this
    double precision, intent(out) :: time
    integer :: bmi_status

    !time = dble(this%model%runinfo%ntimes * this%model%runinfo%dt)  ! time relative to start time (s)
    time = dble(this%model%runinfo%end_datetime)                    ! using unix time (s)
    
    bmi_status = BMI_SUCCESS
  end function sac_end_time

  ! Model current time.
  function sac_current_time(this, time) result (bmi_status)
    class (bmi_sac), intent(in) :: this
    double precision, intent(out) :: time
    integer :: bmi_status

    !time = dble(this%model%runinfo%time_dbl)           ! time from start of run (s)
    time = dble(this%model%runinfo%curr_datetime)    ! unix time (s)
    bmi_status = BMI_SUCCESS
  end function sac_current_time

  ! Model time step.
  function sac_time_step(this, time_step) result (bmi_status)
    class (bmi_sac), intent(in) :: this
    double precision, intent(out) :: time_step
    integer :: bmi_status

    time_step = dble(this%model%runinfo%dt)
    bmi_status = BMI_SUCCESS
  end function sac_time_step

  ! Model time units.
  function sac_time_units(this, units) result (bmi_status)
    class (bmi_sac), intent(in) :: this
    character (len=*), intent(out) :: units
    integer :: bmi_status

    units = "s"
    bmi_status = BMI_SUCCESS
  end function sac_time_units

  ! Advance model by one time step.
  function sac_update(this) result (bmi_status)
    class (bmi_sac), intent(inout) :: this
    integer :: bmi_status

    call advance_in_time(this%model)
    bmi_status = BMI_SUCCESS
  end function sac_update

  ! Advance the model until the given time.
  function sac_update_until(this, time) result (bmi_status)
    class (bmi_sac), intent(inout) :: this
    double precision, intent(in) :: time
    integer :: bmi_status
    double precision :: n_steps_real
    integer :: n_steps, i, s

    ! new code to work with unix time convention
    ! check to see if desired time to advance to is earlier than current time (can't go backwards)
    if (time < this%model%runinfo%curr_datetime) then
       bmi_status = BMI_FAILURE
       return
    end if
    ! otherwise try to advance to end time
    do while ( time < this%model%runinfo%end_datetime )
       s = this%update()
    end do

    ! original code working with time run convention from 0 to n*dt end_time
    !if (time < this%model%runinfo%time_dbl) then
    !   bmi_status = BMI_FAILURE
    !   return
    !end if

    !n_steps_real = (time - this%model%runinfo%time_dbl) / this%model%runinfo%dt
    !n_steps = floor(n_steps_real)
    !do i = 1, n_steps
    !   s = this%update()
    !end do
    !     call update_frac(this, n_steps_real - dble(n_steps)) ! NOT IMPLEMENTED
    
    bmi_status = BMI_SUCCESS
  end function sac_update_until

  ! Get the grid id for a particular variable.
  function sac_var_grid(this, name, grid) result (bmi_status)
    class (bmi_sac), intent(in) :: this
    character (len=*), intent(in) :: name
    integer, intent(out) :: grid
    integer :: bmi_status

    select case(name)
    case('tair', 'precip', 'pet', &     ! input vars
         'qs', 'qg', 'tci', 'eta')      ! output vars
       grid = 0
       bmi_status = BMI_SUCCESS
    case default
       grid = -1
       bmi_status = BMI_FAILURE
    end select
  end function sac_var_grid

  ! The type of a variable's grid.
  function sac_grid_type(this, grid, type) result (bmi_status)
    class (bmi_sac), intent(in) :: this
    integer, intent(in) :: grid
    character (len=*), intent(out) :: type
    integer :: bmi_status

    select case(grid)
    case(0)
       type = "scalar"
       bmi_status = BMI_SUCCESS
!================================ IMPLEMENT WHEN sac DONE IN GRID ======================
!     case(1)
!       type = "uniform_rectilinear"
!        bmi_status = BMI_SUCCESS
    case default
       type = "-"
       bmi_status = BMI_FAILURE
    end select
  end function sac_grid_type

  ! The number of dimensions of a grid.
  function sac_grid_rank(this, grid, rank) result (bmi_status)
    class (bmi_sac), intent(in) :: this
    integer, intent(in) :: grid
    integer, intent(out) :: rank
    integer :: bmi_status

    select case(grid)
    case(0)
       rank = 0
       bmi_status = BMI_SUCCESS
!================================ IMPLEMENT WHEN sac DONE IN GRID ======================
!     case(1)
!        rank = 2
!        bmi_status = BMI_SUCCESS
    case default
       rank = -1
       bmi_status = BMI_FAILURE
    end select
  end function sac_grid_rank

  ! The dimensions of a grid.
  function sac_grid_shape(this, grid, shape) result (bmi_status)
    class (bmi_sac), intent(in) :: this
    integer, intent(in) :: grid
    integer, dimension(:), intent(out) :: shape
    integer :: bmi_status

    select case(grid)
!================================ IMPLEMENT WHEN sac DONE IN GRID ======================
! NOTE: Scalar "grids" do not have dimensions, ie. there is no case(0)
!     case(1)
!        shape(:) = [this%model%n_y, this%model%n_x]
!        bmi_status = BMI_SUCCESS
    case default
       shape(:) = -1
       bmi_status = BMI_FAILURE
    end select
  end function sac_grid_shape

  ! The total number of elements in a grid.
  function sac_grid_size(this, grid, size) result (bmi_status)
    class (bmi_sac), intent(in) :: this
    integer, intent(in) :: grid
    integer, intent(out) :: size
    integer :: bmi_status

    select case(grid)
    case(0)
       size = 1
       bmi_status = BMI_SUCCESS
!================================ IMPLEMENT WHEN sac DONE IN GRID ======================
!     case(1)
!        size = this%model%n_y * this%model%n_x
!        bmi_status = BMI_SUCCESS
    case default
       size = -1
       bmi_status = BMI_FAILURE
    end select
  end function sac_grid_size

  ! The distance between nodes of a grid.
  function sac_grid_spacing(this, grid, spacing) result (bmi_status)
    class (bmi_sac), intent(in) :: this
    integer, intent(in) :: grid
    double precision, dimension(:), intent(out) :: spacing
    integer :: bmi_status

    select case(grid)
!================================ IMPLEMENT WHEN sac DONE IN GRID ======================
! NOTE: Scalar "grids" do not have spacing, ie. there is no case(0)
!     case(1)
!        spacing(:) = [this%model%dy, this%model%dx]
!        bmi_status = BMI_SUCCESS
    case default
       spacing(:) = -1.d0
       bmi_status = BMI_FAILURE
    end select
  end function sac_grid_spacing
!
  ! Coordinates of grid origin.
  function sac_grid_origin(this, grid, origin) result (bmi_status)
    class (bmi_sac), intent(in) :: this
    integer, intent(in) :: grid
    double precision, dimension(:), intent(out) :: origin
    integer :: bmi_status

    select case(grid)
!================================ IMPLEMENT WHEN sac DONE IN GRID ======================
! NOTE: Scalar "grids" do not have coordinates, ie. there is no case(0)
!     case(1)
!        origin(:) = [0.d0, 0.d0]
!        bmi_status = BMI_SUCCESS
    case default
       origin(:) = -1.d0
       bmi_status = BMI_FAILURE
    end select
  end function sac_grid_origin

  ! X-coordinates of grid nodes.
  function sac_grid_x(this, grid, x) result (bmi_status)
    class (bmi_sac), intent(in) :: this
    integer, intent(in) :: grid
    double precision, dimension(:), intent(out) :: x
    integer :: bmi_status

    select case(grid)
    case(0)
       x(:) = [0.d0]
       bmi_status = BMI_SUCCESS
    case default
       x(:) = -1.d0
       bmi_status = BMI_FAILURE
    end select
  end function sac_grid_x

  ! Y-coordinates of grid nodes.
  function sac_grid_y(this, grid, y) result (bmi_status)
    class (bmi_sac), intent(in) :: this
    integer, intent(in) :: grid
    double precision, dimension(:), intent(out) :: y
    integer :: bmi_status

    select case(grid)
    case(0)
       y(:) = [0.d0]
       bmi_status = BMI_SUCCESS
    case default
       y(:) = -1.d0
       bmi_status = BMI_FAILURE
    end select
  end function sac_grid_y

  ! Z-coordinates of grid nodes.
  function sac_grid_z(this, grid, z) result (bmi_status)
    class (bmi_sac), intent(in) :: this
    integer, intent(in) :: grid
    double precision, dimension(:), intent(out) :: z
    integer :: bmi_status

    select case(grid)
    case(0)
       z(:) = [0.d0]
       bmi_status = BMI_SUCCESS
    case default
       z(:) = -1.d0
       bmi_status = BMI_FAILURE
    end select
  end function sac_grid_z

  ! Get the number of nodes in an unstructured grid.
  function sac_grid_node_count(this, grid, count) result(bmi_status)
    class(bmi_sac), intent(in) :: this
    integer, intent(in) :: grid
    integer, intent(out) :: count
    integer :: bmi_status

    select case(grid)
    case(0:1)
       bmi_status = this%get_grid_size(grid, count)
    case default
       count = -1
       bmi_status = BMI_FAILURE
    end select
  end function sac_grid_node_count

  ! Get the number of edges in an unstructured grid.
  function sac_grid_edge_count(this, grid, count) result(bmi_status)
    class(bmi_sac), intent(in) :: this
    integer, intent(in) :: grid
    integer, intent(out) :: count
    integer :: bmi_status

    count = -1
    bmi_status = BMI_FAILURE
  end function sac_grid_edge_count

  ! Get the number of faces in an unstructured grid.
  function sac_grid_face_count(this, grid, count) result(bmi_status)
    class(bmi_sac), intent(in) :: this
    integer, intent(in) :: grid
    integer, intent(out) :: count
    integer :: bmi_status

    count = -1
    bmi_status = BMI_FAILURE
  end function sac_grid_face_count

  ! Get the edge-node connectivity.
  function sac_grid_edge_nodes(this, grid, edge_nodes) result(bmi_status)
    class(bmi_sac), intent(in) :: this
    integer, intent(in) :: grid
    integer, dimension(:), intent(out) :: edge_nodes
    integer :: bmi_status

    edge_nodes(:) = -1
    bmi_status = BMI_FAILURE
  end function sac_grid_edge_nodes

  ! Get the face-edge connectivity.
  function sac_grid_face_edges(this, grid, face_edges) result(bmi_status)
    class(bmi_sac), intent(in) :: this
    integer, intent(in) :: grid
    integer, dimension(:), intent(out) :: face_edges
    integer :: bmi_status

    face_edges(:) = -1
    bmi_status = BMI_FAILURE
  end function sac_grid_face_edges

  ! Get the face-node connectivity.
  function sac_grid_face_nodes(this, grid, face_nodes) result(bmi_status)
    class(bmi_sac), intent(in) :: this
    integer, intent(in) :: grid
    integer, dimension(:), intent(out) :: face_nodes
    integer :: bmi_status

    face_nodes(:) = -1
    bmi_status = BMI_FAILURE
  end function sac_grid_face_nodes

  ! Get the number of nodes for each face.
  function sac_grid_nodes_per_face(this, grid, nodes_per_face) result(bmi_status)
    class(bmi_sac), intent(in) :: this
    integer, intent(in) :: grid
    integer, dimension(:), intent(out) :: nodes_per_face
    integer :: bmi_status

    nodes_per_face(:) = -1
    bmi_status = BMI_FAILURE
  end function sac_grid_nodes_per_face

  ! The data type of the variable, as a string.
  function sac_var_type(this, name, type) result (bmi_status)
    class (bmi_sac), intent(in) :: this
    character (len=*), intent(in) :: name
    character (len=*), intent(out) :: type
    integer :: bmi_status

    select case(name)
    case('tair', 'precip', 'pet',  &     ! input vars
         'qs', 'qg', 'tci', 'eta')       ! output vars
       type = "real"
       bmi_status = BMI_SUCCESS
    case default
       type = "-"
       bmi_status = BMI_FAILURE
    end select
  end function sac_var_type

  ! The units of the given variable.
  function sac_var_units(this, name, units) result (bmi_status)
    class (bmi_sac), intent(in) :: this
    character (len=*), intent(in) :: name
    character (len=*), intent(out) :: units
    integer :: bmi_status

    select case(name)
    case("precip")
       units = "mm"
       bmi_status = BMI_SUCCESS
    case("tair")
       units = "C"
       bmi_status = BMI_SUCCESS
    case("pet")
       units = "mm"
    case("qs")
       units = "mm"
       bmi_status = BMI_SUCCESS
    case("qg")
       units = "mm"
       bmi_status = BMI_SUCCESS
    case("tci")
       units = "mm"
       bmi_status = BMI_SUCCESS
    case("eta")
       units = "mm"
       bmi_status = BMI_SUCCESS
    case default
       units = "-"
       bmi_status = BMI_FAILURE
    end select
  end function sac_var_units

  ! Memory use per array element.
  function sac_var_itemsize(this, name, size) result (bmi_status)
    class (bmi_sac), intent(in) :: this
    character (len=*), intent(in) :: name
    integer, intent(out) :: size
    integer :: bmi_status

    select case(name)
    case("precip")
       size = sizeof(this%model%derived%precip_comb)    ! 'sizeof' in gcc & ifort
       bmi_status = BMI_SUCCESS
    case("tair")
       size = sizeof(this%model%derived%tair_comb)      ! 'sizeof' in gcc & ifort
       bmi_status = BMI_SUCCESS
    case("pet")
       size = sizeof(this%model%derived%pet_comb)       ! 'sizeof' in gcc & ifort
       bmi_status = BMI_SUCCESS
    case("qs")
       size = sizeof(this%model%derived%qs_comb)        ! 'sizeof' in gcc & ifort
       bmi_status = BMI_SUCCESS
    case("qg")
       size = sizeof(this%model%derived%qg_comb)        ! 'sizeof' in gcc & ifort
       bmi_status = BMI_SUCCESS
    case("tci")
       size = sizeof(this%model%derived%tci_comb)      ! 'sizeof' in gcc & ifort
       bmi_status = BMI_SUCCESS
    case("eta")
       size = sizeof(this%model%derived%eta_comb)
    case("roimp")
       size = sizeof(this%model%derived%roimp_comb)
    case("sdro")
       size = sizeof(this%model%derived%sdro_comb)
    case("ssur")
       size = sizeof(this%model%derived%ssur_comb)
    case("sif")
       size = sizeof(this%model%derived%sif_comb)
    case("bfs")
       size = sizeof(this%model%derived%bfs_comb)
    case("bfp")
       size = sizeof(this%model%derived%bfp_comb)
    case default
       size = -1
       bmi_status = BMI_FAILURE
    end select
  end function sac_var_itemsize

  ! The size of the given variable.
  function sac_var_nbytes(this, name, nbytes) result (bmi_status)
    class (bmi_sac), intent(in) :: this
    character (len=*), intent(in) :: name
    integer, intent(out) :: nbytes
    integer :: bmi_status
    integer :: s1, s2, s3, grid, grid_size, item_size

    s1 = this%get_var_grid(name, grid)
    s2 = this%get_grid_size(grid, grid_size)
    s3 = this%get_var_itemsize(name, item_size)

    if ((s1 == BMI_SUCCESS).and.(s2 == BMI_SUCCESS).and.(s3 == BMI_SUCCESS)) then
       nbytes = item_size * grid_size
       bmi_status = BMI_SUCCESS
    else
       nbytes = -1
       bmi_status = BMI_FAILURE
    end if
  end function sac_var_nbytes

  ! The location (node, face, edge) of the given variable.
  function sac_var_location(this, name, location) result (bmi_status)
    class (bmi_sac), intent(in) :: this
    character (len=*), intent(in) :: name
    character (len=*), intent(out) :: location
    integer :: bmi_status
!==================== UPDATE IMPLEMENTATION IF NECESSARY WHEN RUN ON GRID =================
    select case(name)
    case default
       location = "node"
       bmi_status = BMI_SUCCESS
    end select
  end function sac_var_location

  ! Get a copy of a integer variable's values, flattened.
  function sac_get_int(this, name, dest) result (bmi_status)
    class (bmi_sac), intent(in) :: this
    character (len=*), intent(in) :: name
    integer, intent(inout) :: dest(:)
    integer :: bmi_status

    select case(name)
!==================== UPDATE IMPLEMENTATION IF NECESSARY FOR INTEGER VARS =================
!     case("model__identification_number")
!        dest = [this%model%id]
!        bmi_status = BMI_SUCCESS
    case default
       dest(:) = -1
       bmi_status = BMI_FAILURE
    end select
  end function sac_get_int

  ! Get a copy of a real variable's values, flattened.
  function sac_get_float(this, name, dest) result (bmi_status)
    class (bmi_sac), intent(in) :: this
    character (len=*), intent(in) :: name
    real, intent(inout) :: dest(:)
    integer :: bmi_status

    select case(name)
    case("precip")
       dest(1) = this%model%derived%precip_comb
       bmi_status = BMI_SUCCESS
    case("tair")
       dest(1) = this%model%derived%tair_comb
       bmi_status = BMI_SUCCESS
    case("pet")
       dest(1) = this%model%derived%pet_comb
       bmi_status = BMI_SUCCESS
    case("qs")
       dest(1) = this%model%derived%qs_comb
       bmi_status = BMI_SUCCESS
    case("qg")
       dest(1) = this%model%derived%qg_comb
       bmi_status = BMI_SUCCESS
    case("tci")
       dest(1) = this%model%derived%tci_comb
       bmi_status = BMI_SUCCESS
    case("eta")
       dest(1) = this%model%derived%eta_comb
       bmi_status = BMI_SUCCESS
    case("roimp")
       dest(1) = this%model%derived%roimp_comb
       bmi_status = BMI_SUCCESS
    case("sdro")
       dest(1) = this%model%derived%sdro_comb
       bmi_status = BMI_SUCCESS
    case("ssur")
       dest(1) = this%model%derived%ssur_comb
       bmi_status = BMI_SUCCESS
    case("sif")
       dest(1) = this%model%derived%sif_comb
       bmi_status = BMI_SUCCESS
    case("bfs")
       dest(1) = this%model%derived%bfs_comb
       bmi_status = BMI_SUCCESS
    case("bfp")
       dest(1) = this%model%derived%bfp_comb
       bmi_status = BMI_SUCCESS
    case default
       dest(:) = -1.0
       bmi_status = BMI_FAILURE
    end select
    ! NOTE, if vars are gridded, then use:
    ! dest = reshape(this%model%temperature, [this%model%n_x*this%model%n_y]) 
  end function sac_get_float

  ! Get a copy of a double variable's values, flattened.
  function sac_get_double(this, name, dest) result (bmi_status)
    class (bmi_sac), intent(in) :: this
    character (len=*), intent(in) :: name
    double precision, intent(inout) :: dest(:)
    integer :: bmi_status

    !==================== UPDATE IMPLEMENTATION IF NECESSARY FOR DOUBLE VARS =================

    select case(name)
    case default
       dest(:) = -1.d0
       bmi_status = BMI_FAILURE
    end select
  end function sac_get_double

! !=================== get_value_ptr functions not implemented yet =================

   ! Get a reference to an integer-valued variable, flattened.
   function sac_get_ptr_int(this, name, dest_ptr) result (bmi_status)
     class (bmi_sac), intent(in) :: this
     character (len=*), intent(in) :: name
     integer, pointer, intent(inout) :: dest_ptr(:)
     integer :: bmi_status
     type (c_ptr) :: src
     integer :: n_elements

 !==================== UPDATE IMPLEMENTATION IF NECESSARY FOR INTEGER VARS =================

     select case(name)
     case default
        bmi_status = BMI_FAILURE
     end select
   end function sac_get_ptr_int

   ! Get a reference to a real-valued variable, flattened.
   function sac_get_ptr_float(this, name, dest_ptr) result (bmi_status)
     class (bmi_sac), intent(in) :: this
     character (len=*), intent(in) :: name
     real, pointer, intent(inout) :: dest_ptr(:)
    integer :: bmi_status, status
     type (c_ptr) :: src
     integer :: n_elements, gridid

     select case(name)
     case default
        bmi_status = BMI_FAILURE
     end select
   end function sac_get_ptr_float

   ! Get a reference to an double-valued variable, flattened.
   function sac_get_ptr_double(this, name, dest_ptr) result (bmi_status)
     class (bmi_sac), intent(in) :: this
     character (len=*), intent(in) :: name
     double precision, pointer, intent(inout) :: dest_ptr(:)
     integer :: bmi_status
     type (c_ptr) :: src
     integer :: n_elements

 !==================== UPDATE IMPLEMENTATION IF NECESSARY FOR DOUBLE VARS =================\

     select case(name)
     case default
        bmi_status = BMI_FAILURE
     end select
   end function sac_get_ptr_double

   ! Get values of an integer variable at the given locations.
   function sac_get_at_indices_int(this, name, dest, inds) &
        result (bmi_status)
     class (bmi_sac), intent(in) :: this
     character (len=*), intent(in) :: name
     integer, intent(inout) :: dest(:)
     integer, intent(in) :: inds(:)
     integer :: bmi_status
     type (c_ptr) src
     integer, pointer :: src_flattened(:)
     integer :: i, n_elements

     select case(name)
     case default
        bmi_status = BMI_FAILURE
     end select
   end function sac_get_at_indices_int

   ! Get values of a real variable at the given locations.
   function sac_get_at_indices_float(this, name, dest, inds) &
        result (bmi_status)
     class (bmi_sac), intent(in) :: this
    character (len=*), intent(in) :: name
     real, intent(inout) :: dest(:)
     integer, intent(in) :: inds(:)
     integer :: bmi_status
     type (c_ptr) src
     real, pointer :: src_flattened(:)
     integer :: i, n_elements

     select case(name)
     case default
        bmi_status = BMI_FAILURE
     end select
   end function sac_get_at_indices_float

   ! Get values of a double variable at the given locations.
   function sac_get_at_indices_double(this, name, dest, inds) &
        result (bmi_status)
     class (bmi_sac), intent(in) :: this
     character (len=*), intent(in) :: name
     double precision, intent(inout) :: dest(:)
     integer, intent(in) :: inds(:)
     integer :: bmi_status
     type (c_ptr) src
     double precision, pointer :: src_flattened(:)
     integer :: i, n_elements

     select case(name)
     case default
        bmi_status = BMI_FAILURE
     end select
   end function sac_get_at_indices_double

 ! Set new integer values.
  function sac_set_int(this, name, src) result (bmi_status)
    class (bmi_sac), intent(inout) :: this
    character (len=*), intent(in) :: name
    integer, intent(in) :: src(:)
    integer :: bmi_status

    !==================== UPDATE IMPLEMENTATION IF NECESSARY FOR INTEGER VARS =================

    select case(name)
!     case("model__identification_number")
!        this%model%id = src(1)
!        bmi_status = BMI_SUCCESS
    case default
       bmi_status = BMI_FAILURE
    end select
  end function sac_set_int

  ! Set new real values.
  function sac_set_float(this, name, src) result (bmi_status)
    class (bmi_sac), intent(inout) :: this
    character (len=*), intent(in) :: name
    real, intent(in) :: src(:)
    integer :: bmi_status

    select case(name)
    case("precip")
       this%model%derived%precip_comb = src(1)
       bmi_status = BMI_SUCCESS
    case("tair")
       this%model%derived%tair_comb = src(1)
       bmi_status = BMI_SUCCESS
    case("pet")
       this%model%derived%pet_comb = src(1)
       bmi_status = BMI_SUCCESS
    case("qs")
       this%model%derived%qs_comb = src(1)
       bmi_status = BMI_SUCCESS
    case("qg")
       this%model%derived%qg_comb = src(1)
       bmi_status = BMI_SUCCESS
    case("tci")
       this%model%derived%tci_comb = src(1)
       bmi_status = BMI_SUCCESS
    case("eta")
       this%model%derived%eta_comb = src(1)
       bmi_status = BMI_SUCCESS
    case("roimp")
       this%model%derived%roimp_comb = src(1)
       bmi_status = BMI_SUCCESS
    case("sdro")
       this%model%derived%sdro_comb = src(1)
       bmi_status = BMI_SUCCESS
    case("ssur")
       this%model%derived%ssur_comb = src(1)
       bmi_status = BMI_SUCCESS
    case("sif")
       this%model%derived%sif_comb = src(1)
       bmi_status = BMI_SUCCESS
    case("bfs")
       this%model%derived%bfs_comb = src(1)
       bmi_status = BMI_SUCCESS
    case("bfp")
       this%model%derived%bfp_comb = src(1)
       bmi_status = BMI_SUCCESS
    case default
       bmi_status = BMI_FAILURE
    end select
    ! NOTE, if vars are gridded, then use:
    ! this%model%temperature = reshape(src, [this%model%n_y, this%model%n_x])
  end function sac_set_float

  ! Set new double values.
  function sac_set_double(this, name, src) result (bmi_status)
    class (bmi_sac), intent(inout) :: this
    character (len=*), intent(in) :: name
    double precision, intent(in) :: src(:)
    integer :: bmi_status

    !==================== UPDATE IMPLEMENTATION IF NECESSARY FOR DOUBLE VARS =================

    select case(name)
    case default
       bmi_status = BMI_FAILURE
    end select
  end function sac_set_double

   ! Set integer values at particular locations.
   function sac_set_at_indices_int(this, name, inds, src) &
        result (bmi_status)
     class (bmi_sac), intent(inout) :: this
     character (len=*), intent(in) :: name
     integer, intent(in) :: inds(:)
     integer, intent(in) :: src(:)
     integer :: bmi_status
     type (c_ptr) dest
     integer, pointer :: dest_flattened(:)
     integer :: i

     select case(name)
     case default
        bmi_status = BMI_FAILURE
     end select
   end function sac_set_at_indices_int

   ! Set real values at particular locations.
   function sac_set_at_indices_float(this, name, inds, src) &
        result (bmi_status)
     class (bmi_sac), intent(inout) :: this
     character (len=*), intent(in) :: name
     integer, intent(in) :: inds(:)
     real, intent(in) :: src(:)
     integer :: bmi_status
     type (c_ptr) dest
     real, pointer :: dest_flattened(:)
     integer :: i

     select case(name)
     case default
        bmi_status = BMI_FAILURE
     end select
   end function sac_set_at_indices_float

   ! Set double values at particular locations.
   function sac_set_at_indices_double(this, name, inds, src) &
        result (bmi_status)
     class (bmi_sac), intent(inout) :: this
     character (len=*), intent(in) :: name
     integer, intent(in) :: inds(:)
     double precision, intent(in) :: src(:)
     integer :: bmi_status
     type (c_ptr) dest
     double precision, pointer :: dest_flattened(:)
     integer :: i

     select case(name)
     case default
        bmi_status = BMI_FAILURE
     end select
   end function sac_set_at_indices_double

!   ! A non-BMI helper routine to advance the model by a fractional time step.
!   subroutine update_frac(this, time_frac)
!     class (bmi_sac), intent(inout) :: this
!     double precision, intent(in) :: time_frac
!     real :: time_step
!
!     if (time_frac > 0.0) then
!        time_step = this%model%dt
!        this%model%dt = time_step*real(time_frac)
!        call advance_in_time(this%model)
!        this%model%dt = time_step
!     end if
!   end subroutine update_frac
!
!   ! A non-BMI procedure for model introspection.
!   subroutine print_model_info(this)
!     class (bmi_sac), intent(in) :: this
!
!     call print_info(this%model)
!   end subroutine print_model_info
#ifdef NGEN_ACTIVE
  function register_bmi(this) result(bmi_status) bind(C, name="register_bmi")
   use, intrinsic:: iso_c_binding, only: c_ptr, c_loc, c_int
   use iso_c_bmif_2_0
   implicit none
   type(c_ptr) :: this ! If not value, then from the C perspective `this` is a void**
   integer(kind=c_int) :: bmi_status
   !Create the model instance to use
   type(bmi_sac), pointer :: bmi_model
   !Create a simple pointer wrapper
   type(box), pointer :: bmi_box

   !allocate model
   allocate(bmi_sac::bmi_model)
   !allocate the pointer box
   allocate(bmi_box)

   !associate the wrapper pointer the created model instance
   bmi_box%ptr => bmi_model

   if( .not. associated( bmi_box ) .or. .not. associated( bmi_box%ptr ) ) then
    bmi_status = BMI_FAILURE
   else
    !Return the pointer to box
    this = c_loc(bmi_box)
    bmi_status = BMI_SUCCESS
   endif
 end function register_bmi
#endif

end module bmi_sac_module
