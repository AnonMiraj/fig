program path_test
    use fig_path
    implicit none
    type(path) :: my_path
    call my_path%moveTo(10.0, 10.0)
    call my_path%lineTo(50.0, 20.0)
    call my_path%bezierCurveTo(30.0, 40.0, 60.0, 50.0, 80.0, 30.0)
    call my_path%quadraticCurveTo(100.0, 50.0, 120.0, 80.0)
    call my_path%ellipticalArcTo(40.0, 20.0, 30.0, 1, 0, 150.0, 100.0)
    call my_path%closePath()
    print *,my_path%path_string
    

end program path_test

