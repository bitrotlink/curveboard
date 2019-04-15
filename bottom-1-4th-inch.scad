include <flatboard-lib.scad>;

mount_hole_diameter = 4*0.85; // Tight fit for screwing standoffs into wood

difference() {
  plate();
  mounts();
  * translate([75, -100])
      arduino_mounts();
}

//test_plate();
