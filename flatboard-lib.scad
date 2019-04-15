//Numbers for linear distances are in millimeters.

include <kbd-lib.scad>;

curved = false;
two_D = true;
omissions = 2;
render_kbd_tent_and_yaw = false;
render_kbd_pitch = false;
display_keyswitches = true;
accurate_holepunch = false;
avoid_steep_overhang = false;
parallel_thumb_key_qnty = 2;
render_kbd_frame = false;
render_index_finger_tertiary_key = false;
raise_secondary_thumb_keys = false;
diode_pinholes = true;
cutout_only_global = true;
hand_spacing= 62;
upper_center_key_offset = -12;
lower_center_key_offset = -90;
thumb_pinch_key_x_hack = 0;
column_palm_length = 210;
column_palm_offset = -55;
column_palm_width = 140;
center_rect_height = sin(kbd_half_yaw)*column_palm_width/2;
projection_cuts = true;
mx_keyhole_x_y = 14;
mount_hole_diameter = 4;
header_pin_spacing = 2.54;
header_pin_size = 0.635; //Standard header pin. Should be 0.635*0.85 for tight fit, but laser cutter at Fuse can't make reliable hole that small in 1/8" plywood.
arduino_mount_hole_size = 2.5*0.85;
minkowski_radius = 0;

module center_key(cutout_only=false) {
  translate([0, 0, -common_frame_thickness/2]) //For flat keyboard, put top surface of keyframe at z=0; to match what happens in key_column() (in keywell())
    rotate([0, 0, 45])
      keyframe(cutout_only=cutout_only_global);
}

module plate_1() {
     translate([0, column_palm_offset]) {
       translate([-1*column_palm_width, -1/2*column_palm_length+minkowski_radius])
         square([3/2*column_palm_width - minkowski_radius, column_palm_length - minkowski_radius*2]);
     }
 }

module plate_trim_1() {
         rotate([0, 0, -kbd_half_yaw*2])
         translate([-10,70])
         square([50,50]);
}

module plate_trim_2() {
         rotate([0, 0, -kbd_half_yaw])
         translate([115,-140])
         square([50,50]);
}

module plate_trim_3() {
         translate([-150, 57])
         square([300,25]);
         translate([-150, -225])
         square([300,50+minkowski_radius]);
}

module center_mounts() {
    translate([0, 30])
      circle(r=mount_hole_diameter/2);
    translate([0, -65])
      circle(r=mount_hole_diameter/2);
}

module mounts() {
  for(i=[0,1])
  translate([hand_spacing*(i==0?1:-1), 0, 0])
  mirror([i, 0, 0])
  rotate([0, 0, kbd_half_yaw]) {
    translate([64, -130])
      circle(r=mount_hole_diameter/2);
    translate([-70, -153])
      circle(r=mount_hole_diameter/2);
    translate([64, 26])
      circle(r=mount_hole_diameter/2);
    translate([0, -55])
      circle(r=mount_hole_diameter/2);
    translate([64, -55])
      circle(r=mount_hole_diameter/2);
  }
  center_mounts();
}

module header_pins() {
    for(i=[0,1])
    translate([hand_spacing*(i==0?1:-1), 0, 0])
    mirror([i, 0, 0])
    rotate([0, 0, kbd_half_yaw]) {
      for(i=[0:19])
          translate([i*header_pin_spacing - 3, -65])
            square([header_pin_size, header_pin_size]);
      }
}

//Specs from: https://www.flickr.com/photos/johngineer/5484250200/sizes/o/in/photostream/
module arduino_mounts() {
    translate([hand_spacing, 0, 0])
    rotate([0, 0, 180+kbd_half_yaw]) {
        translate([0, 0])
          circle(r=arduino_mount_hole_size/2);
        translate([-1.3, -15.24 - 27.94 - 5.08])
          circle(r=arduino_mount_hole_size/2);
        translate([50.8, -15.24])
          circle(r=arduino_mount_hole_size/2);
        translate([50.8, -15.24 - 27.94])
          circle(r=arduino_mount_hole_size/2);
      }    
}


module plate_2() {
    for(i=[0,1])
    translate([hand_spacing*(i==0?1:-1), 0, 0])
    mirror([i, 0, 0])
    rotate([0, 0, kbd_half_yaw]) {
       difference() {
           plate_1();
           plate_trim_1();
           plate_trim_2();
           }
      }
     translate([0, 30])
        square([200, 25], center=true);
}

module plate() {
       difference() {
           plate_2();
           plate_trim_3();
       }
}

module plate_uncut_with_minkowski() {
    minkowski() {
       difference() {
           plate_2();
           plate_trim_3();
       }
       circle(r=minkowski_radius);        
    }
}

module cutouts() {
for(i=[0,1])
translate([hand_spacing*(i==0?1:-1), 0, 0])
mirror([i, 0, 0])
rotate([0, 0, kbd_half_yaw]) {
     projection(cut = true)
     translate([0, 0, BS]) {
       keywell(cutout_only=cutout_only_global);
       translate([0, 0, -common_frame_thickness/2])
         thumbplate(cutout_only=cutout_only_global);
     }
  }
  projection(cut = projection_cuts)
  translate([0, upper_center_key_offset, BS])
  center_key(cutout_only=cutout_only_global);

  projection(cut = projection_cuts)
  translate([0, lower_center_key_offset, BS])
  center_key(cutout_only=cutout_only_global);
}

module test_plate() {
difference() {
intersection() {
  {
    translate([0, 60, 0])
    mirror([0, 0, 0])
    rotate([0, 0, kbd_half_yaw]) {

    square([150,40],center=true);
    }
  }
  {
    translate([0, 60, 0])
    mirror([1, 0, 0])
    rotate([0, 0, kbd_half_yaw]) {

    square([150,40],center=true);
    }
  }
  }
  translate([0, 76, 0])
    square([200,40],center=true);

    translate([40, 40, 0])
       square([30, 20]);
    translate([-70, 40, 0])
       square([30, 20]);

    translate([-6, 47, 0])
       square([0.635*1.05, 0.635*1.05]);
    translate([-3, 47, 0])
       square([0.635, 0.635]);
    translate([0, 47, 0])
       square([0.635*0.95, 0.635*0.95]);
    translate([3, 47, 0])
       square([0.635*0.9, 0.635*0.9]);
    translate([6, 47, 0])
       square([0.635*0.85, 0.635*0.85]);
    translate([9, 47, 0])
       square([0.635*0.8, 0.635*0.8]);

    translate([-6, 44, 0])
      circle(r=0.644*1.05/2);
    translate([-3, 44, 0])
      circle(r=0.644/2);
    translate([0, 44, 0])
      circle(r=0.644*0.95/2);
    translate([3, 44, 0])
      circle(r=0.644*0.9/2);
    translate([6, 44, 0])
      circle(r=0.644*0.85/2);
    translate([9, 44, 0])
      circle(r=0.644*0.8/2);

    translate([-9, 44, 0])
      circle(r=0.511/2);

    translate([-6, 52, 0])
      circle(r=4*1.05/2);
    translate([0, 52, 0])
      circle(r=4/2);
    translate([6, 52, 0])
      circle(r=4*0.95/2);
    translate([12, 52, 0])
      circle(r=4*0.90/2);
    translate([18, 52, 0])
      circle(r=4*0.85/2);
    translate([24, 52, 0])
      circle(r=4*0.8/2);

    translate([-12, 52, 0])
      circle(r=3*1.05/2);
    translate([-16, 52, 0])
      circle(r=3/2);
    translate([-20, 52, 0])
      circle(r=3*0.95/2);
    translate([-24, 52, 0])
      circle(r=3*0.9/2);
    translate([-28, 52, 0])
      circle(r=3*0.85/2);
    translate([-32, 52, 0])
      circle(r=3*0.8/2);

  }
}

//difference() {
//  plate();
//  cutouts();
//  center_mounts();
//}
