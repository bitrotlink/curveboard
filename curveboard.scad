include <kbd-lib.scad>;

curved = true;
two_D = false;
omissions = 2;
render_kbd_tent_and_yaw = true;
render_kbd_pitch = true;
display_keyswitches = true;
accurate_holepunch = false;
avoid_interfering_supports = false;
render_thumb_pinch_key = true;
render_index_finger_tertiary_key = false;
hand_spacing= 60;

for(i=[0,1])
translate([hand_spacing*(i==0?1:-1), 0, 0])
mirror([i, 0, 0])
kbd_half();
