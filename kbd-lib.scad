//Numbers for linear distances are in millimeters. Angles are in degrees.

curved = false; //Set to true for curved keyboard, or false for flat
two_D = false; //Produce 2D model instead of 3D
avoid_interfering_supports = false; //Set to true to extend keyswitch hole depth so Slicer doesn't put support material in annoying places
enable_injection_moldability = true; //Avoid cavities that would prevent straight-pull injection molding
conserve_plastic = false; //Set to true to conserve some plastic when 3D printing keywell without full keyboard frame
omissions = 0; //Omission level. For faster rendering during development. Set to 0 for production, 1 to omit hulls, 2 to also omit hull boundary objects, and 3 to also omit all inter-keyframe filling.
render_kbd_tent_and_yaw = false; //Set to true to render tent and yaw of keyboard relative to user's body
render_kbd_pitch = true;
render_kbd_frame = true;
display_keyswitches = false; //Set to true to display keyswitches as openscad ghosts
accurate_holepunch = false; //Set to true to punch holes after rendering entire frame, instead of just punching per-keyswitch. Ensures that any accidental structural overlap with keyswitch or keycap is punched out.
$fa=0.05;
$wf=0.05;
$fn=45;

BS = 0.04; //B.S. needed to make oscad not whine about non-manifold structures. Use 0.001 to minimize distortion for production, and 0.04 to eliminate rendering artifacts is oscad during development.
nominal_thickness = BS; //Where only 0D, 1D, or 2D object is needed for hull boundary, but oscad requires 3D object.
oversize = 0; //Set to 0.1 for FDM 3D printer when keyframe surface is on print bed, since unlike laser cutter, it produces holes a little smaller than spec in that case.
infin = 200; //Effectively infinite

//Numbers here are from specs
//mx_keyswitch_x_y = 15.6+oversize; //Keyswitch is square.
mx_keyswitch_x_y = 17+oversize; //Keyswitch is square. //FIXME: this is just to save 3D printing plastic and time. Switch back to 15.6.
mx_keyhole_x_y = 14+oversize; //Spec is 14mm±0.05. Hole is square.
mx_frame_thickness = 1.524; //Spec is 1.524mm (0.06in exactly) ± 0.1mm
//Top of housing and top of plunger are approximately the same heights as bottom and top of DSA-profile keycap, respectively.
mx_frame_to_plunger_top = 10.2-mx_frame_thickness; //Distance from top of frame to top of plunger
mx_frame_to_housing_top = 6.6-mx_frame_thickness; //Distance from top of frame to top of housing
mx_frame_to_pin = 8.3; //Distance from bottom of frame to tip of pins
matias_keyswitch_x = 17.2+oversize;
matias_keyswitch_y = 14+oversize;
matias_keyhole_x = 15.5+oversize;
matias_keyhole_y = 12.8+oversize;
matias_frame_thickness = 1.1;
keysize = 19; //Standard for full-size keyboards. Inter-key center-to-center distance. Square grid.
leg_height = (8.3-mx_frame_thickness)+0.5; //MX switch protrudes 8.3mm down from top surface of frame. 0.5mm is for clearance, and is arbitrary. MX length is enough for Matias, so use same for both for simplicity.
key_travel_distance = 4;

//Numbers here are from measurements, since there are no specs
mx_latch_offset = 2.75; //Distance from center of side of housing to start of top-housing latch (which is used to remove top of housing from bottom of housing)
mx_latch_width = 3.45;
DSA_cap_top_width = 12;

common_frame_thickness = 3; //Arbitrary. Compromise between frame flex and cost and time (3D printing time, or injection molding cooling time) of production. FIXME: 2 might be adequate.
extended_frame_thickness = mx_frame_to_pin; //Used only for test 3D prints of keywell and thumbplate; not used in production. Extends keyhole sides enough to accomodate full keyswitch, so that 3D printing vertical support material under it doesn't interfere with keyswitch for keyswitches that aren't vertical.
support_material_thickness = infin; //Used for 3D printing support structure to avoid steep overhangs.

frame_border = 1; //Arbitrary
key_row_qnty = 4; //For single-row test frame

row_max = 2; //Top row of keywell
row_min = -2; //Bottom row of keywell
column_row_limit = [3, 3, 3, 3, 3, 2]; //One higher than topmost row in given column
column_row_min = [-1, -2, -2, -2, -2, -1]; //Bottommost row in given column
use_pseudo_secondaries = false; //Use pseudo-secondary columns for index and little fingers generated with the home columns (an alternative to making the secondaries regular columns)
index_finger_home_column = 1;
middle_finger_column = index_finger_home_column+1;
little_finger_home_column = middle_finger_column+2;
index_finger_secondary_column = index_finger_home_column-1;
little_finger_secondary_column = little_finger_home_column+1;
column_qnty = 6; //The y_roll, y_twist, z_base_height, column_row_limit, column_row_min, and y_translation array lengths must equal this.

//Curvature and positioning variables.
x_pitch = curved?15:0; //Inter-row angle (i.e. rotation around x-axis), in degrees. Optimal is about 10-15°.
y_roll = curved?[8, 4, 0, 1, 4, 0]:[0, 0, 0, 0, 0, 0]; //Rotation (on right hand) of home keys around y-axis.
y_twist = curved?[0, 0, 0, -0.5, -1, 0]:[0, 0, 0, 0, 0, 0]; //Multiplied by row number and added to column's y_roll to produce row-specific roll
z_base_height = curved?[5, 5, 0, 3, 6, 6]:[0, 0, 0, 0, 0, 0]; //Base heights (before adjusting for column position and roll) of index, middle, ring, and little finger home keys above center key (middle finger home key).
function z_height(column) = //Total heights of home keys, including adjustments.
     !curved?0:
     (z_base_height[column]
     //I don't know why the following expression works. I just hacked it until the rendering looked right.
     +(!use_pseudo_secondaries&&(column==index_finger_secondary_column||column==little_finger_secondary_column)?
       sign(middle_finger_column-column)*keysize*sin(y_roll[column]):0));

y_translation = [0, 0, 0, 0, -keysize/2, -keysize/2]; //Translation of columns along y-axis

draft_angle = 4; //For injection molding

keycap_spacing = 0; //Gap between keycaps, to keep them from touching
key_y_spacing = mx_frame_to_plunger_top*tan(x_pitch)+keycap_spacing; //Prevent keys from being so close together that keycaps interfere with each other

kbd_half_yaw = 15; //Finger column yaw angle. Total angle between the two keyboard halves is twice this.
kbd_tent = curved?15:0;
kbd_pitch = curved?5:0;
palm_pitch= curved?15:0;
pitch_palmrest_with_keywell = false; //When pitching keywell, true if palmrest pitches to match; false to only vertically translate palmrest to match
thumb_top_x_offset = -0.5*keysize; //x-offset of right-hand thumb key that's higher along y-axis than thumb's home key
thumb_pitch = 0;
thumb_roll = curved?-kbd_tent:0;
thumb_yaw = 12;

thumbplate_z_offset = z_height(index_finger_home_column);
parallel_thumb_key_qnty = 3; //Number of parallel double-length thumb keys
render_thumb_pinch_key = true; //Whether to create thumb pinch key
thumb_pinch_key_hack = 0; //Hack to avoid interference from main keywell for thumb pinch key when using extended frame for 3D printing
thumb_x_hack = 2; //Hack to eliminate a little superfluous x-axis spacing of thumb plate, because I'm tired of doing the trig
first_secondary_thumb_key = 2; //Number of first thumb key that counts as secondary (space key is zero)
match_roll_secondary_thumb_keys = true; //Option to roll secondaries the same as primaries
add_yaw_secondary_thumb_keys = 3; //Additional yaw for each thumb key relative to the previous one
raise_secondary_thumb_keys = true; //Raise secondary thumb keys above z-height of thumb plane
middle_finger_y_bottom = //Almost the same as for thumbplate
     (keysize+key_y_spacing)*(-1/2 - cos(x_pitch) - cos(2*x_pitch));

render_index_finger_tertiary_key = true;
tertiary_finger_3D_printing_join_hack = false;

palmrest_x_width = 120;
flat_palmrest_y_height = 80;
total_palmrest_y_height = 105; //Includes non-flat portion
palmrest_right_margin = 10; //Past right egde of touchpad
palmrest_y_bottom_margin = 5; //Past y-bottom of touchpad
touchpad_clearance = 1/2;
touchpad_x_width = 100+touchpad_clearance; //MS standard is 105, but forcepad is 100 (device total is 111)
touchpad_y_height = 68+touchpad_clearance; //MS standard is 65, but forcepad is 68 (device total is 74)

skirt_cylinder_radius = 0.2; //Must be less than (keysize-keyswitch_x)/4 to avoid spilling into MX latch cutout.

//Variables set for MX switches
keyswitch_frame_thickness = mx_frame_thickness;
keyswitch_x = mx_keyswitch_x_y;
keyswitch_y = mx_keyswitch_x_y;
keyhole_x = mx_keyhole_x_y;
keyhole_y = mx_keyhole_x_y;
mx_latches = true;

palmrest_y_bottom =
     -total_palmrest_y_height*cos(palm_pitch) //Height in x-y plane of palmrest itself
     +middle_finger_y_bottom
     +y_translation[little_finger_home_column]; //Extend from little finger's y translation, since that's farthest down

palmrest_left_edge = -keysize/2-2*keysize*cos(y_roll[0]); //Align left side to left side of index finger secondary column
palmrest_right_edge = palmrest_x_width + palmrest_left_edge;

kbd_right_edge = palmrest_right_edge + keyswitch_frame_thickness; //The right at the palmrest; actual right is slightly farther right due to draft angle on the right side of the keyboard.

kbd_y_bottom = palmrest_y_bottom - keyswitch_frame_thickness; //The bottom at the palmrest; actual bottom is slightly lower due to draft angle on the y-bottom side of the keyboard.

kbd_y_top_clearance = 3;
kbd_z_top_clearance = 3;
kbd_y_top = -middle_finger_y_bottom + kbd_y_top_clearance; //Middle finger y-top is symmetric (through x-axis) to middle finger y-bottom

kbd_z_bottom_clearance = 0; //Clearance between lowest point on lowest keyswitch and the top of the bottom plate of the keyboard frame.

kbd_z_bottom = min(-keyswitch_frame_thickness-mx_frame_to_pin, thumbplate_z_offset-(parallel_thumb_key_qnty-1/2)*keysize*sin(-thumb_roll)+key_travel_distance*(parallel_thumb_key_qnty-first_secondary_thumb_key)-mx_frame_to_pin*cos(-thumb_roll)+(render_kbd_pitch?(kbd_y_bottom+total_palmrest_y_height)*sin(kbd_pitch):0)) - kbd_z_bottom_clearance - common_frame_thickness;

frame_y_top_wall_height = z_height(middle_finger_column)
     +keysize*(row_max+1)*tan(row_max*x_pitch)
     +kbd_z_top_clearance-kbd_z_bottom;

palmrest_z_offset = (z_height(little_finger_home_column)
		 +2*keysize*sin(y_roll[little_finger_home_column])*cos(2*x_pitch))
		+keysize*sin(x_pitch)+keysize*sin(2*x_pitch)
		-total_palmrest_y_height*sin(palm_pitch)
		+(render_kbd_pitch&&!pitch_palmrest_with_keywell?
		  (palmrest_y_bottom+total_palmrest_y_height):0)*tan(kbd_pitch);

frame_y_bottom_wall_height = -palmrest_z_offset;
left_wall_offset = -81; //FIXME: parameterize based on position of thumb keys. If kbd_half_yaw is 10, then use left_wall_offset of -86.

module right_iso_triangle (base, thickness) {
     difference() {
	  cube([base, base, thickness]);
	  translate([base, 0, -BS]) rotate([0, 0, 45]) cube([base, base*1.5, thickness+2*BS]); //1.5 is round up of √2.
     }
}

module keyhole(keyswitch_frame_thickness, keyswitch_x, keyswitch_y, keyhole_x, keyhole_y, mx_latches, two_D=false) { //Subtracted from start frame to produce hole. mx_latches is option to provide clearance for top-housing latches, so top housing can be removed without removing entire keyswitch from the frame. two_D, if true, is option to produce only the hole for the keyswitch, without the larger hole under it to create lip for keyswitch housing latches to catch on; this is useful only for 2D laser cuts in material with thickness keyswitch_frame_thickness.
     translate([0, 0, keyswitch_frame_thickness+((avoid_interfering_supports||enable_injection_moldability)?(keyswitch_x-keyhole_x)/2:0)])
	  if(two_D==false) cube([keyswitch_x, keyswitch_y, (common_frame_thickness+(mx_frame_to_pin-(keyswitch_x-keyhole_x)/2)-keyswitch_frame_thickness)+BS]); //Restrict frame thickness to keyswitch spec in area of keyswitch, so keyswitch can latch into place using frame latches on bottom of housing when inserted into hole. (This is unrelated to the top-housing latches that mx_latch_offset, mx_latch_width, and mx_latches provide clearance for.)
     translate([(keyswitch_x-keyhole_x)/2, (keyswitch_y-keyhole_y)/2, -BS]) {
	  cube([keyhole_x, keyhole_y, common_frame_thickness+mx_frame_to_pin+3*BS]); //Punch square hole through entire frame
	  if(avoid_interfering_supports||enable_injection_moldability)
	       translate([keyhole_x/2, keyhole_y/2, keyswitch_frame_thickness+2*BS])
		    rotate(45) cylinder(h=(keyswitch_x-keyhole_x)/2+BS,
					r1=keyhole_x/sqrt(2),
					r2=keyswitch_x/sqrt(2),
					$fn=4); //Punch pyramidal hole through frame
     }
     translate([0, 0, -mx_frame_to_housing_top])
	  cube([keyswitch_x, keyswitch_y, mx_frame_to_housing_top]);
     if(display_keyswitches)
	  % mirror([0, 0, 1]) { //Because the keyhole module is upside down
	       translate([(keyswitch_x-keyhole_x)/2, (keyswitch_y-keyhole_y)/2, -mx_frame_to_pin-mx_frame_thickness])
		    cube([keyhole_x, keyhole_y, mx_frame_to_pin+mx_frame_thickness]);
	       cube([keyswitch_x, keyswitch_y, mx_frame_to_housing_top]);
	       translate([keyswitch_x/2, keyswitch_y/2, mx_frame_to_housing_top])
		    rotate(45) cylinder(h=mx_frame_to_plunger_top-mx_frame_to_housing_top,
					r1=keyswitch_x/sqrt(2),
					r2=DSA_cap_top_width/sqrt(2),
					$fn=4);
	  }
     if(mx_latches) { //Punch holes through entire frame for clearance for unlatching MX top-housing latches
	  translate([keyswitch_x/2, keyswitch_y/2, 0])
	       for(i=[0,180])
		    rotate([0, 0, i])
			 for(j=[0,1])
			      translate([keyhole_x/2-BS, j==0?mx_latch_offset:-mx_latch_offset-mx_latch_width, -BS])
				   cube([(keyswitch_x-keyhole_x)/2+BS, mx_latch_width, common_frame_thickness+mx_frame_to_pin+3*BS]);
     }
}

module maybe_hull() {
     if(omissions==1)
	  union() {
	       for(i=[0:$children-1])
		    children(i);
	  }
     else if(omissions==0)
	  hull() {
	       for(i=[0:$children-1])
		    children(i);
	  }
}

module keyframe_skirt(skirt_type, ysize=1) { //Skirt around keyframe, to avoid gaps due to use of keyframes on a curve.
     //ysize is y-height.
     //skirt_types:
     //0: no skirt
     //1 and 2: unused
     //3: ytop, 4: ybottom,
     //5: right-ztop, 6: right-zbottom, 7: left-ztop, 8: left-zbottom
     //9: right-ybottom-zbottom, 10: left-ybottom-zbottom, 11: right-ybottom-ztop, 12: left-ybottom-ztop (ybottom corners)
     //13: right-ytop-ztop, 14: left-ytop-ztop, 15: right-ytop-zbottom, 16: left-ytop-zbottom (ytop corners)
     if(skirt_type==3) color([1, 0, 0]) keyframe_skirt_ytop(ysize);
     if(skirt_type==4) color([0, 1, 0]) mirror([0, 1, 0]) keyframe_skirt_ytop(ysize);
     if(skirt_type==5) color([1, 1, 0]) keyframe_skirt_right(1, ysize);
     if(skirt_type==6) color([1, 1, 0]) keyframe_skirt_right(-1, ysize);
     if(skirt_type==7) color([0, 0, 1]) mirror([1, 0, 0]) keyframe_skirt_right(1, ysize);
     if(skirt_type==8) color([0, 0, 1]) mirror([1, 0, 0]) keyframe_skirt_right(-1, ysize);
     if(skirt_type==9) color([1, 1, 1]) keyframe_skirt_right_corner(-1, -1, ysize);
     if(skirt_type==10) color([1, 1, 1]) mirror([1, 0, 0]) keyframe_skirt_right_corner(-1, -1, ysize);
     if(skirt_type==11) color([1, 1, 1]) keyframe_skirt_right_corner(-1, 1, ysize);
     if(skirt_type==12) color([1, 1, 1]) mirror([1, 0, 0]) keyframe_skirt_right_corner(-1, 1, ysize);
     if(skirt_type==13) color([1, 1, 1]) keyframe_skirt_right_corner(1, 1, ysize);
     if(skirt_type==14) color([1, 1, 1]) mirror([1, 0, 0]) keyframe_skirt_right_corner(1, 1, ysize);
     if(skirt_type==15) color([1, 1, 1]) keyframe_skirt_right_corner(1, -1, ysize);
     if(skirt_type==16) color([1, 1, 1]) mirror([1, 0, 0]) keyframe_skirt_right_corner(1, -1, ysize);
}

module keyframe_skirt_ytop(ysize=1) {
     for(i=[-1, 1])
	  translate([0, keysize*ysize/2, i*(common_frame_thickness/2-skirt_cylinder_radius)])
	       rotate([0, 90, 0]) cylinder(h=keysize, r=skirt_cylinder_radius, center=true);
}

module keyframe_skirt_right(z_pos, ysize=1) { //z_pos is 1 for top, -1 for bottom
     //Fully within keyframe; used only for generation of hull that extends beyond keyframe
     translate([keysize/2-skirt_cylinder_radius, 0, z_pos*(common_frame_thickness/2-skirt_cylinder_radius)])
	  rotate([90, 0, 0]) cylinder(h=keysize*ysize, r=skirt_cylinder_radius, center=true);
}

module keyframe_skirt_right_corner(y_pos, z_pos, ysize=1) { //Fully within keyframe; used only for generation of hull that extends beyond keyframe
     translate([keysize/2-skirt_cylinder_radius-BS, y_pos*(keysize*ysize-nominal_thickness)/2, z_pos*(common_frame_thickness/2-skirt_cylinder_radius-BS)])
	  rotate([90, 0, 0]) cylinder(h=nominal_thickness, r=skirt_cylinder_radius+BS, center=true);
}

module keyframe(skirt_type=0, skirt_only=false, cutout=true, cutout_only=false, ysize=1) { //See keyframe_skirt for skirt_type values. skirt_only: produce only skirt, not the frame segment. cutout: true for frame cutout for keyswitch, false for blank frame segment (no cutout). cutout_only: false for standard keyframe, true for only the hole (to subtract from something else). ysize: keycap y-height relative to standard (keysize).
     if(cutout_only==true) {
	  rotate([0, 180, 0]) //Keyhole is upside down for ease of 3D printing; flip it right-side up here.
	       translate([-keyswitch_x/2, -keyswitch_y/2, -common_frame_thickness/2])
	       keyhole(keyswitch_frame_thickness, keyswitch_x, keyswitch_y, keyhole_x, keyhole_y, mx_latches, two_D=two_D&&!curved);
     }
     else {
	  if(skirt_only==false) {
	       if (cutout==true) difference() { //Frame segment with cutout
			 union() {
			      cube([keysize+BS, keysize*ysize+BS, common_frame_thickness], center=true);
			      if(avoid_interfering_supports)
				   translate([0, 0, -extended_frame_thickness/2])
					cube([keysize+BS, keysize*ysize+BS, extended_frame_thickness], center=true);
			 }
			 rotate([0, 180, 0]) //Keyhole is upside down for ease of 3D printing; flip it right-side up here.
			      translate([-keyswitch_x/2, -keyswitch_y/2, -common_frame_thickness/2])
			      keyhole(keyswitch_frame_thickness, keyswitch_x, keyswitch_y, keyhole_x, keyhole_y, mx_latches);
		    }
	       else cube([keysize+BS, keysize*ysize+BS, common_frame_thickness], center=true);
	  }
	  if(skirt_type!=0) keyframe_skirt(skirt_type, ysize);
     }
}

module map_adjust_key_column(direction) { //Translate along and rotate around the column's curve
     for(i=[0:$children-1])
	  translate([0, direction*(keysize/2+key_y_spacing), 0]) //Evenly align to centered key
	       rotate([direction*x_pitch, 0, 0])
	       translate([0, direction*keysize/2, 0]) //Put edge on x-axis
	       children(i);
}

function x_offset_adjust(column) =  //Adjusted x offset of column to account for roll, so columns don't overlap.
     // The 2014 release of openscad apparently doesn't even have «let» statements. Beyond retarded.
     sign(column-middle_finger_column)
     *((sin(abs(y_roll[column]+abs(y_twist[column])*(y_roll[column]>0?1:-1)
		-y_roll[sign(column-middle_finger_column)==1?column-1:column+1]
		-abs(y_twist[sign(column-middle_finger_column)==1?column-1:column+1])
		*(y_roll[sign(column-middle_finger_column)==1?column-1:column+1]>0?1:-1)))
	*(common_frame_thickness+mx_frame_to_plunger_top))<keycap_spacing?keycap_spacing:
       (sin(abs(y_roll[column]+abs(y_twist[column])*(y_roll[column]>0?1:-1)
		-y_roll[sign(column-middle_finger_column)==1?column-1:column+1]
		-abs(y_twist[sign(column-middle_finger_column)==1?column-1:column+1])
		*(y_roll[sign(column-middle_finger_column)==1?column-1:column+1]>0?1:-1)))
	*(common_frame_thickness+mx_frame_to_plunger_top)))
     +(column==middle_finger_column?
       0:x_offset_adjust(sign(column-middle_finger_column)==1?
			 (column-1):(column+1)));

module key_column(column, row=0, add_secondary=false, skirt_type=0, skirt_only=false, exclusive_row=false, exclusive_row_number=0, cutout_only=false, cutout=true) {
     //row is starting row.
     //add_secondary is option to add column parallel to primary one.
     //See keyframe_skirt for skirt_type values, and keyframe for skirt_only.
     //exclusive_row is option to produce only the keyframe specified by exclusive_row_number, and use key_column_gen's recursion just to position and orient that keyframe. This is needed because of openscad's brain damage (no way for one pass through the recursion to return a list of multiple geometric objects, because geometric objects aren't values that variables can be bound to).
     translate([(column-middle_finger_column)*keysize //Base position of column, with middle finger at center column
		+x_offset_adjust(column),
		y_translation[column], +z_height(column)])
	  key_column_gen(column, row, add_secondary, skirt_type, skirt_only, exclusive_row, exclusive_row_number, cutout_only, cutout);
}

module key_column_gen(column, row=0, add_secondary=false, skirt_type=0, skirt_only=false, exclusive_row=false, exclusive_row_number=0, cutout_only=false, cutout=true) {
     if(exclusive_row==false || row==exclusive_row_number)
	  translate([0, 0, -common_frame_thickness/2]) //Put top surface of keyframe at z=0
	       rotate([0, y_roll[column]+row*y_twist[column], 0]) {
		  if(column_row_min[column]<=row&&row<column_row_limit[column]
		     &&(use_pseudo_secondaries==false||
			(column>=index_finger_home_column&&column<=little_finger_home_column)))
		       keyframe(skirt_type, skirt_only, cutout, cutout_only);
		  //Add parallel keyframe in column left of right index finger's home column, or right of right little finger's home column
		  if(add_secondary && column==index_finger_home_column && row>=column_row_min[column-1] && row<column_row_limit[column-1]) //But omit bottom-row keyframe in column to left
		       translate([-keysize, 0, 0]) keyframe(skirt_type<5 //Left/right skirts not needed here
							    && (row>column_row_min[column-1]
								|| (skirt_type!=2 && skirt_type!=4)) //Omit skirt adjacent to omitted keyframe
							    ? skirt_type:0, skirt_only, cutout, cutout_only);
		  if(add_secondary && column==index_finger_home_column && row==1) { //Tertiary column for index finger, with single key
		       translate([-2*keysize, -keysize/2, keysize/2*sin(x_pitch)])
			    keyframe(0, skirt_only, cutout, cutout_only);
		    if(skirt_only==false)
			 for(y=[-1,1])
			      translate([-2*keysize, y*keysize*3/4-keysize/2, keysize/2*sin(x_pitch)+(avoid_interfering_supports?-extended_frame_thickness/2:0)])
				   *cube([keysize+BS, keysize/2+BS, common_frame_thickness+(avoid_interfering_supports?extended_frame_thickness:0)], center=true); //Add frame area under the double-length key, same as for double-length thumb keys. FIXME: add this back (remove star) for final; omitting temporarily just to avoid wasting plastic and time when 3D printing. FIXME: also reduce this to 1.5x, not 2x.
		  }
		  if(add_secondary && column==little_finger_home_column && row>=column_row_min[column+1] && row<column_row_limit[column+1]) //Omit top- and bottom-row keyframes in column to right
		       translate([keysize, 0, 0]) keyframe(skirt_type<5 //Left/right skirts not needed here
							    && (row>column_row_min[column+1] || (skirt_type!=2 && skirt_type!=4)) //Omit skirt adjacent to omitted keyframe
							    && (row<column_row_limit[column+1]-1 || (skirt_type!=1 && skirt_type!=3)) //Omit skirt adjacent to omitted keyframe
							   ? skirt_type:0, skirt_only, cutout, cutout_only);
	       }
     //Recurse in both directions from home row
     if(row==0) {
	  if(row_max>0) map_adjust_key_column(1) key_column_gen(column, 1, add_secondary, skirt_type, skirt_only, exclusive_row, exclusive_row_number, cutout_only, cutout);
	  if(row_min<0) map_adjust_key_column(-1) key_column_gen(column, -1, add_secondary, skirt_type, skirt_only, exclusive_row, exclusive_row_number, cutout_only, cutout);
     }
     if(row!=0 && row>row_min && row<row_max)
	  map_adjust_key_column(sign (row))
	       key_column_gen(column, row>0 ? row+1 : row-1, add_secondary, skirt_type, skirt_only, exclusive_row, exclusive_row_number, cutout_only, cutout);
}

module index_finger_tertiary_key(skirt_type=0, skirt_only=false, cutout_only=false) { //Real tertiary column for index finger, with one key.
     translate([-keysize*cos(y_roll[index_finger_home_column])
		-2*keysize*cos(y_roll[index_finger_secondary_column])
		+x_offset_adjust(index_finger_secondary_column)
		-sin(y_roll[index_finger_secondary_column])*(common_frame_thickness+mx_frame_to_plunger_top),
		keysize*7/8+key_y_spacing/2,
		z_height(index_finger_secondary_column)+keysize*sin(y_roll[index_finger_secondary_column])]) {
	  rotate([x_pitch, y_roll[index_finger_secondary_column], 0]) {
	       keyframe(cutout_only=cutout_only, ysize=conserve_plastic?1:1.5);
	       if(tertiary_finger_3D_printing_join_hack)
		    translate([keysize*17/32, 0, 0])
			 cube([keysize/8, keysize, common_frame_thickness], center=true);
	  }
     }
}

module keywell(cutout_only=false, cutout=true) {
     for(column=[0:column_qnty-1]) {
	  key_column(column, cutout_only=cutout_only, add_secondary=use_pseudo_secondaries, cutout=cutout); //Produce keyframes
	  //Add inter-keyframe filling, as Schlemiel the painter to work around openscad's brain damage
	  for(row=[row_min:row_max]) {
	       if(row<row_max) {
		    maybe_hull()
			 for(st=[3:4])
			      key_column(column, 0, use_pseudo_secondaries, st, true, true,
					 row+(st<4?0:1), cutout_only=cutout_only, cutout=cutout);
	       }
	       if(column<column_qnty-1) {
		    maybe_hull() {
			 for(st=[5:8])
			      key_column(column+(st<7?0:1), 0, use_pseudo_secondaries, st, true, true,
					 row, cutout_only=cutout_only, cutout=cutout);
		    }
		    maybe_hull() {
			 if(row<row_max) {
			      for(st=[9:16])
				   key_column(column+(st%2==0?1:0), 0, use_pseudo_secondaries, st, true, true,
					      row+(st<13?1:0), cutout_only=cutout_only, cutout=cutout);
			 }
		    }
	       }
	  }
     }
     if(!use_pseudo_secondaries&&render_index_finger_tertiary_key) index_finger_tertiary_key(cutout_only=cutout_only);
}

module thumbplate(cutout_only=false) {
     //See the whiteboard geometry-fu for translation expression derivations
     translate([-1/2*keysize-3/2*keysize*cos(y_roll[0]) //Primary x positioning relative to keywell
		-mx_frame_to_pin*2*sin((-thumb_roll+y_roll[0])/2)/thumb_x_hack, //x offset to avoid interference caused by roll
		-(keysize)*(1/2+cos(x_pitch)+1/2*cos(2*x_pitch)) //Primary y positioning
		-key_y_spacing
		-1/2*keysize //y offset to account for double-length thumb key caps, with mount in middle of cap
		-(keysize/2*sin(thumb_yaw)) //y offset to avoid intereference caused by yaw.
		-mx_frame_to_pin*2*sin((-row_min*x_pitch+thumb_pitch)/2), //y offset to avoid interference caused by pitch
		thumbplate_z_offset]) {
	  rotate([thumb_pitch, thumb_roll, 0]) {
	       rotate([0, 0, thumb_yaw]) {
		    for(x=[0:parallel_thumb_key_qnty-1]) {
			 translate([-x*keysize-(keysize*2)*sin(x*add_yaw_secondary_thumb_keys),
				    -(keysize*2)*sin(x*add_yaw_secondary_thumb_keys),
				    (raise_secondary_thumb_keys&&x>=first_secondary_thumb_key)?
				    key_travel_distance*(1+x-first_secondary_thumb_key):0])
			      rotate([0, (!match_roll_secondary_thumb_keys&&x>=first_secondary_thumb_key) ? -thumb_roll:0,
					  x*add_yaw_secondary_thumb_keys])
			      keyframe(cutout_only=cutout_only, ysize=conserve_plastic?1:2); //These are for the three vertical (along y-axis) double-length thumb keys
		    }
		    translate([-keysize+keysize*sin(thumb_yaw), keysize*3/2+mx_frame_to_plunger_top*sin(x_pitch), key_travel_distance])
			 rotate([x_pitch, 0, 0])
			 keyframe(cutout_only=cutout_only); //For top thumb key
	       }
	  }
	  translate([keysize*(1+sin(thumb_yaw))+mx_frame_to_plunger_top*sin(-thumb_roll),
		     -keysize/2
		     //-key_y_spacing //Unneeded
                     //-mx_frame_to_pin*sin(x_pitch) //Unneeded
		     -thumb_pinch_key_hack,
		     keysize*sin(-thumb_roll)+keysize*sin(x_pitch)]) { //For thumb pinch key
	       rotate([-x_pitch, thumb_roll, 0]) {
		    if(render_thumb_pinch_key) keyframe(cutout_only=cutout_only);
		    else keyframe(cutout=false);
	       }
	  }
     }
}

module palmrest(cutout_only=false) {
     translate([palmrest_left_edge, palmrest_y_bottom, palmrest_z_offset]) {
	  rotate([palm_pitch, 0, 0]) {
	       translate([0, 0, -common_frame_thickness]) {
		    if(cutout_only==false) difference() {
			 cube([palmrest_x_width, flat_palmrest_y_height, common_frame_thickness]);
			 translate([palmrest_x_width-touchpad_x_width-palmrest_right_margin,
				    palmrest_y_bottom_margin, -BS])
			      cube([touchpad_x_width, touchpad_y_height, common_frame_thickness+2*BS]);
		    }
		    else translate([palmrest_x_width-touchpad_x_width-palmrest_right_margin,
				    palmrest_y_bottom_margin, -BS])
			      cube([touchpad_x_width, touchpad_y_height, common_frame_thickness+2*BS]);
	       }
	  }
     }
}

module y_top_wall_positive() {
     translate([palmrest_left_edge, //y_top wall
		kbd_y_top-common_frame_thickness/2+sin(draft_angle)*frame_y_top_wall_height,
		kbd_z_bottom])
	  rotate([draft_angle, 0, 0])
	  cube([palmrest_x_width, common_frame_thickness,
		frame_y_top_wall_height]);
}

module y_top_wall_negative() {
     translate([-infin/2, kbd_y_top+common_frame_thickness/2+sin(draft_angle)*frame_y_top_wall_height, kbd_z_bottom-BS])
	  rotate([draft_angle, 0, 0])
	  translate([0, 0, -infin/2])
	  cube([infin, infin, infin]);
}

module y_bottom_wall_positive() {
     translate([palmrest_left_edge,
		kbd_y_bottom+common_frame_thickness/2-sin(draft_angle)*frame_y_bottom_wall_height,
		kbd_z_bottom])
	  rotate([-draft_angle, 0, 0])
	  cube([palmrest_x_width, common_frame_thickness,
		frame_y_bottom_wall_height]);
}

module y_bottom_wall_negative() {
     translate([palmrest_left_edge-BS,
		kbd_y_bottom+common_frame_thickness/2-sin(draft_angle)*frame_y_bottom_wall_height,
		kbd_z_bottom])
	  rotate([-draft_angle, 0, 0])
	  translate([0, -infin, -infin/2])
	  cube([palmrest_x_width+BS, infin+BS, infin]);
}

module concaver() {
     translate([-kbd_y_bottom, 0])
	  rotate(render_kbd_pitch?2*kbd_pitch:0)
	  translate([0, -kbd_z_bottom+keysize/sin(x_pitch)])
	  circle(r=keysize/(2*tan(x_pitch/2)), $fn=360);
}

module right_wall() {
     translate([palmrest_left_edge+palmrest_x_width-common_frame_thickness-BS,
		kbd_y_bottom+common_frame_thickness/2, kbd_z_bottom])
	  rotate([90, 0, 90]) {
	  linear_extrude(common_frame_thickness) {
	       difference() {
		    square([kbd_y_top-kbd_y_bottom, frame_y_top_wall_height]);
		    translate([kbd_y_top-kbd_y_bottom-common_frame_thickness/2+sin(draft_angle)*frame_y_top_wall_height+BS, -BS])
			 rotate(draft_angle)
			 square([infin, infin]);
		    translate([-BS, frame_y_bottom_wall_height-BS])
			 rotate(palm_pitch)
			 square([infin, infin]);
		    concaver();
	       }
	  }
     }
}

module left_wall_positive() {
     translate([left_wall_offset, 0, kbd_z_bottom])
	  rotate([0, -kbd_tent, -kbd_half_yaw])
	  translate([0, -infin, 0])
	  cube([common_frame_thickness, infin*2, frame_y_top_wall_height/cos(kbd_tent)]);
}

module left_wall_negative() {
     translate([left_wall_offset+BS, 0, kbd_z_bottom-BS])
	  rotate([0, -kbd_tent, -kbd_half_yaw])
	  translate([-infin, -infin, 0])
	  cube([infin, infin*2, frame_y_top_wall_height/cos(kbd_tent)-kbd_z_bottom+BS]);
}

module left_y_top_corner_wall_negative() {
     translate([palmrest_left_edge,
		kbd_y_top+(common_frame_thickness/2
		+sin(draft_angle)*frame_y_top_wall_height)/sin(45), kbd_z_bottom-BS])
	  rotate([draft_angle, 0, 45])
	  translate([-infin/2, 0, 0])
	  cube([infin, infin, infin]);
}

module left_y_top_corner_wall_positive() {
     translate([palmrest_left_edge,
		kbd_y_top-common_frame_thickness/2+sin(draft_angle)*frame_y_top_wall_height,
		kbd_z_bottom])
	  rotate([draft_angle, 0, 45])
	  translate([-infin/2, 0, 0])
	  cube([infin, common_frame_thickness, frame_y_top_wall_height]);
}

module right_y_top_corner_wall_negative() {
     translate([3.5*keysize+x_offset_adjust(little_finger_secondary_column)+BS,
		3/2*keysize+y_translation[little_finger_secondary_column]+key_y_spacing+BS, -infin/2])
	  rotate([0, 0, 45])
	  translate([0, -infin/2, 0])
	  cube([infin, infin, infin]);
}

module right_y_top_corner_wall_positive() {
     difference() {
	  translate([3.5*keysize+x_offset_adjust(little_finger_secondary_column),
		     3/2*keysize+y_translation[little_finger_secondary_column]+key_y_spacing, kbd_z_bottom+BS]) {
	       rotate([0, 0, 45]) {
		    translate([0, -infin/6, 0])
			 cube([common_frame_thickness, infin/2, frame_y_top_wall_height-BS]);
	       }
	  }
	  y_top_wall_negative();
	  translate([palmrest_left_edge+palmrest_x_width+BS, -infin/2, -infin/2])
	       cube([infin, infin, infin]);
	  translate([palmrest_left_edge+palmrest_x_width-common_frame_thickness-infin/2,
		     kbd_y_bottom+common_frame_thickness/2, kbd_z_bottom-BS])
	       rotate([90, 0, 90]) {
	       linear_extrude(infin) concaver();
	  }
     }
}

module left_y_bottom_corner_wall_positive() {
     translate([palmrest_left_edge,
		kbd_y_bottom+common_frame_thickness/2-sin(draft_angle)*frame_y_bottom_wall_height,
		kbd_z_bottom])
     rotate([0, draft_angle, 45-kbd_half_yaw])
	  translate([0, -infin, 0])
	  cube([common_frame_thickness, 2*infin, frame_y_bottom_wall_height]);
}

module left_y_bottom_corner_wall_negative() {
     translate([palmrest_left_edge,
		kbd_y_bottom+common_frame_thickness/2-sin(draft_angle)*frame_y_bottom_wall_height,
		kbd_z_bottom])
     rotate([0, draft_angle, 45-kbd_half_yaw])
	  translate([-infin+BS, -infin, -infin/2])
	  cube([infin, 2*infin, infin]);
}

module kbd_frame_wall_boundaries() {
     right_y_top_corner_wall_negative();
     left_y_top_corner_wall_negative();
     y_top_wall_negative();
     left_wall_negative();
     left_y_bottom_corner_wall_negative();
     y_bottom_wall_negative();
}

module kbd_frame_walls() {
     difference() {
	  union() {
	       y_top_wall_positive();
	       right_wall();
	       left_wall_positive();
	       left_y_top_corner_wall_positive();
	       left_y_bottom_corner_wall_positive();
	  }
	  kbd_frame_wall_boundaries();
     }
     y_bottom_wall_positive();
     right_y_top_corner_wall_positive();
}

module bottom_plate() {
     translate([palmrest_left_edge, palmrest_y_bottom, kbd_z_bottom])
	  cube([palmrest_x_width, kbd_y_top-kbd_y_bottom, keyswitch_frame_thickness]);
}

module bottom_flattener() {
     translate([-infin*2, -infin*2, kbd_z_bottom-infin])
	  cube([infin*4, infin*4, keyswitch_frame_thickness+infin]);
}

module top_flattener() {
     translate([0, kbd_y_top+common_frame_thickness/cos(draft_angle)+2*BS, 0])
	  rotate([kbd_pitch, 0, 0])
	  translate([-infin*2, -infin*2, frame_y_top_wall_height+kbd_z_bottom-3*BS])
	  cube([infin*4, infin*4, infin]);
}

module kbd_half_raw(cutout_only=false) {
     if(cutout_only) {
	  rotate([render_kbd_pitch?kbd_pitch:0, 0, 0]) {
	       keywell(cutout_only=cutout_only);
	       thumbplate(cutout_only=cutout_only);
	       if(pitch_palmrest_with_keywell)
		    palmrest(cutout_only=cutout_only);
	  }
	  if(render_kbd_frame&&!cutout_only)
	       kbd_frame_walls();
	  if(render_kbd_pitch&&!pitch_palmrest_with_keywell)
	       palmrest(cutout_only=cutout_only);
     } else if (accurate_holepunch) {
	  difference() {
	       union() {
		    rotate([render_kbd_pitch?kbd_pitch:0, 0, 0]) {
			 keywell(cutout=false);
			 //TODO: Add cutout=false argument here, and corresponding parameter, for each of the following.
			 thumbplate();
			 if(pitch_palmrest_with_keywell) palmrest();
		    }
		    if(render_kbd_frame&&!cutout_only) kbd_frame_walls();
		    if(render_kbd_pitch&&!pitch_palmrest_with_keywell) palmrest();
	       }
	       union() {
		    rotate([render_kbd_pitch?kbd_pitch:0, 0, 0]) {
			 keywell(cutout_only=true);
			 thumbplate(cutout_only=true);
			 if(pitch_palmrest_with_keywell) palmrest(cutout_only=true);
		    }
		    if(render_kbd_frame&&!cutout_only) kbd_frame_walls();
		    if(render_kbd_pitch&&!pitch_palmrest_with_keywell) palmrest(cutout_only=true);
	       }
	  }
     } else {
	  rotate([render_kbd_pitch?kbd_pitch:0, 0, 0]) {
	       keywell();
	       thumbplate();
	       if(pitch_palmrest_with_keywell) palmrest();
	  }
	  if(render_kbd_frame&&!cutout_only) kbd_frame_walls();
	  if(render_kbd_pitch&&!pitch_palmrest_with_keywell) palmrest();
     }
}

module kbd_half(cutout_only=false) {
     rotate([0, render_kbd_tent_and_yaw?kbd_tent:0, 0]) {
	  rotate([0, 0, render_kbd_tent_and_yaw?kbd_half_yaw:0]) {
	       difference() {
		    kbd_half_raw(cutout_only);
		    bottom_flattener();
		    top_flattener();
	       }
	  }
     }
}
