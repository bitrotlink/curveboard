include <flatboard-lib.scad>;

module plate_trim_palms() {
  for(i=[0,1]) {
    translate([0*hand_spacing*(i==0?1:-1), -210, 0])
    mirror([i, 0, 0])
    rotate([0, 0, kbd_half_yaw]) {
         square([200,90]);
    }
    translate([2*hand_spacing*(i==0?1:-1)*1.15, -195, 0])
    mirror([i, 0, 0])
    rotate([0, 0, kbd_half_yaw]) {
         square([200,145]);
    }
  }
}

difference() {
  plate();
  mounts();
  cutouts();
  plate_trim_palms();
}

//test_plate();
