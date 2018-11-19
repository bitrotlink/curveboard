include <flatboard-lib.scad>;

mx_keyhole_x_y = 16;

difference() {
  plate();
  cutouts();
  center_mounts();
  header_pins();
}

//test_plate();
