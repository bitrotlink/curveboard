include <flatboard-lib.scad>;

mx_keyhole_x_y = 16;

difference() {
  plate();
  mounts();
  cutouts();
  header_pins();
}

//test_plate();
