// modified functions from Melisa Akan

function drawPro(canvas, width, height, rad, angle, fb, demo, correct, response){
	x_center=width/2;
	y_center=height/2;

	var ctx = canvas.getContext('2d');
	//draw half circle
	ctx.beginPath();
	ctx.arc(x_center, y_center, rad, 0*(Math.PI), 1*(Math.PI), true);
	ctx.lineTo(x_center + rad, y_center);
	ctx.fillStyle = '#008000'; //green
	ctx.fill();
	ctx.closePath();

	//draw wedge
	ctx.beginPath();
	ctx.moveTo(x_center, y_center);
	//clockwise direction if false
	//counter clockwise if true
	//the wedge length is fixed to 150
	ctx.arc(x_center, y_center, 150, 0*(Math.PI), angle,true);
	ctx.closePath();
	ctx.fillStyle = '#FFC000'; //yellow
  ctx.fill();
	ctx.closePath();
	//console.log(response); console.log(correct); console.log(fb)
	if(!demo){
		ctx.font='24px sans-serif';
		if(fb){
			if(response=="u"){
				if(correct==1){
					ctx.fillStyle='green';
					ctx.fillText("Correct! This turtle IS in the subspecies.", x_center-.5*ctx.measureText("Correct! This turtle IS in the subspecies.").width, height*.6);
				}else if(correct==0){
					ctx.fillStyle='red';
					ctx.fillText('Incorrect. This turtle IS NOT in the subspecies.', x_center-.5*ctx.measureText('Incorrect. This turtle IS NOT in the subspecies.').width, height*.6)
				}
			}else if(response=="i"){
				if(correct==1){
					ctx.fillStyle='green';
					ctx.fillText("Correct! This turtle IS NOT in the subspecies.", x_center-.5*ctx.measureText("Correct! This turtle IS NOT in the subspecies.").width, height*.6);
				}else if(correct==0){
					ctx.fillStyle='red';
					ctx.fillText('Incorrect. This turtle IS in the subspecies.', x_center-.5*ctx.measureText('Incorrect. This turtle IS in the subspecies.').width, height*.6)
				}
			}
		}else{
			ctx.font='24px sans-serif';
			ctx.fillStyle='black';
			ctx.fillText("Is this turtle in the subspecies? (U=YES, I=NO)", x_center-.5*ctx.measureText("Is this turtle in the subspecies? (U=YES, I=NO)").width, height*.6);
			//ctx.fillText("J or K?", width*.48, height*.7);
		}
	}

};

function drawTwoPros(c, width, height, left_r, left_a, right_r, right_a, fb=false, correct=false, trial_type=null, jkl_correct=null, wedgeLen=150){
	//draw two protractors side by side, for similarity rating trials
	ctx=c.getContext('2d');

	var interval=60;

	var radii = [left_r, right_r]; //radius of the turtle on the left, and turtle on the right
	var angles = [convertToRadian(left_a), convertToRadian(right_a)]; //^^ angles in radians

	var x_center = width/2;
	var y_center = height/2;

	var x_center_left = x_center-(.5*interval+wedgeLen);
	var x_center_right = x_center+(.5*interval+right_r); //center of the right turtle

	console.log("left radius = ",left_r, "left angle=",left_a)
	console.log("right radius = ",right_r, "right angle=",right_a)



	// DRAW FIRST PROTRACTOR
	//draw half circle
	ctx.beginPath();
	ctx.arc(x_center_left, y_center, radii[0], 0*(Math.PI), 1*(Math.PI), true);
	ctx.lineTo(x_center_left + radii[0], y_center);
	ctx.fillStyle = '#008000'; //green
	ctx.fill();
	ctx.closePath();

	//draw wedge
	ctx.beginPath();
	ctx.moveTo(x_center_left, y_center);
	//clockwise direction if false
	//counter clockwise if true
	//the wedge length is fixed to 150
	ctx.arc(x_center_left, y_center, 150, 0*(Math.PI), angles[0],true);
	ctx.closePath();
	ctx.fillStyle = '#FFC000'; //yellow
  ctx.fill();
	ctx.closePath();

	// DRAW SECOND PROTRACTOR
	//draw half circle
	ctx.beginPath();
	ctx.arc(x_center_right, y_center, radii[1], 0*(Math.PI), 1*(Math.PI), true);
	ctx.lineTo(x_center_right + radii[1], y_center);
	ctx.fillStyle = '#008000'; //green
	ctx.fill();
	ctx.closePath();

	//draw wedge
	ctx.beginPath();
	ctx.moveTo(x_center_right, y_center);
	//clockwise direction if false
	//counter clockwise if true
	//the wedge length is fixed to 150
	ctx.arc(x_center_right, y_center, 150, 0*(Math.PI), angles[1],true);
	ctx.closePath();
	ctx.fillStyle = '#FFC000'; //yellow
  ctx.fill();
	ctx.closePath();

	ctx.font='24px sans-serif';
	ctx.fillStyle='black';
	ctx.fillText('J', x_center_left-.5*ctx.measureText('J').width, height*.55);
	ctx.fillText('K', x_center_right-.5*ctx.measureText('K').width, height*.55);

	if(fb){
		if(trial_type!="attraction"){
			if(correct){
				ctx.fillStyle='green';
				ctx.fillText('CORRECT! Turtle '+jkl_correct.toUpperCase()+ ' is most likely to be in the subspecies.', x_center-.5*ctx.measureText('CORRECT! '+jkl_correct.toUpperCase()+ ' is most likely to be in the subspecies.').width, height*.6);
			}else{
				ctx.fillStyle='red';
				ctx.fillText('INCORRECT. Turtle '+jkl_correct.toUpperCase()+' is most likely to be in the subspecies.', x_center-.5*ctx.measureText('INCORRECT. Turtle '+jkl_correct.toUpperCase()+' is most likely to be in the subspecies.').width, height*.6)
			}
		}else{
			console.log("HERE")
			ctx.fillStyle='black';
			ctx.fillText('Thank you.',x_center-.5*ctx.measureText('Thank you').width,height*.6);
		}
	}else{
		ctx.fillText("Which turtle is most likely to be in the subspecies?", x_center-.5*ctx.measureText("Which turtle is most likely to be in the subspecies?").width, height*.6);
		ctx.fillText("J or K?", x_center-.5*ctx.measureText("J or K?"), height*.7);
	}
};

function drawThreePros(c, width, height, left_r, left_a, center_r, center_a, right_r, right_a, fb=false, correct=false, trial_type=null, jkl_correct=null, wedgeLen=150){
	// canvas
	ctx=c.getContext('2d');

	var interval=60;

	var radii = [left_r, center_r, right_r]; //radius of the turtle on the left, and turtle on the right
	var angles = [convertToRadian(left_a), convertToRadian(center_a), convertToRadian(right_a)]; //^^ angles in radians

	x_center=width/2;
	y_center=height/2

	var x_center_center = x_center; // center of the middle turtyle
	var x_center_left = x_center-(interval+center_r+wedgeLen); //center of the left turtle
	var x_center_right = x_center+(interval+right_r+wedgeLen); //center of the right turtle

	// console.log("x_center_left=",x_center_left);
	// console.log("x_center_center=",x_center_center);
	// console.log("x_center_right=",x_center_right);
	// console.log("interval=",interval);
	console.log("left radius = ",left_r, "left angle=",left_a)
	console.log("center radius = ",center_r, "center angle=",center_a)
	console.log("right radius = ",right_r, "right angle=",right_a)


	// DRAW FIRST PROTRACTOR
	//draw half circle
	ctx.beginPath();
	ctx.arc(x_center_left, y_center, radii[0], 0*(Math.PI), 1*(Math.PI), true);
	ctx.lineTo(x_center_left + radii[0], y_center);
	ctx.fillStyle = '#008000'; //green
	ctx.fill();
	ctx.closePath();

	//draw wedge
	ctx.beginPath();
	ctx.moveTo(x_center_left, y_center);
	//clockwise direction if false
	//counter clockwise if true
	//the wedge length is fixed to 150
	ctx.arc(x_center_left, y_center, 150, 0*(Math.PI), angles[0],true);
	ctx.closePath();
	ctx.fillStyle = '#FFC000'; //yellow
  ctx.fill();
	ctx.closePath();

	// DRAW SECOND PROTRACTOR
	//draw half circle
	ctx.beginPath();
	ctx.arc(x_center_center, y_center, radii[1], 0*(Math.PI), 1*(Math.PI), true);
	ctx.lineTo(x_center_center + radii[1], y_center);
	ctx.fillStyle = '#008000'; //green
	ctx.fill();
	ctx.closePath();

	//draw wedge
	ctx.beginPath();
	ctx.moveTo(x_center_center, y_center);
	//clockwise direction if false
	//counter clockwise if true
	//the wedge length is fixed to 150
	ctx.arc(x_center_center, y_center, 150, 0*(Math.PI), angles[1],true);
	ctx.closePath();
	ctx.fillStyle = '#FFC000'; //yellow
  ctx.fill();
	ctx.closePath();
	ctx.font='24px sans-serif';

	// DRAW THIRD PROTRACTOR
	//draw half circle
	ctx.beginPath();
	ctx.arc(x_center_right, y_center, radii[2], 0*(Math.PI), 1*(Math.PI), true);
	ctx.lineTo(x_center_right + radii[2], y_center);
	ctx.fillStyle = '#008000'; //green
	ctx.fill();
	ctx.closePath();

	//draw wedge
	ctx.beginPath();
	ctx.moveTo(x_center_right, y_center);
	//clockwise direction if false
	//counter clockwise if true
	//the wedge length is fixed to 150
	ctx.arc(x_center_right, y_center, 150, 0*(Math.PI), angles[2],true);
	ctx.closePath();
	ctx.fillStyle = '#FFC000'; //yellow
  ctx.fill();
	ctx.closePath();

	// draw stimulus names
	ctx.fillStyle='black';
	ctx.fillText('J', x_center_left-.5*ctx.measureText('J').width, height*.55);
	ctx.fillText('K', x_center_center-.5*ctx.measureText('K').width, height*.55);
	ctx.fillText('L', x_center_right-.5*ctx.measureText('L').width, height*.55);

	if(fb){
		if(trial_type!="attraction"){
			console.log("not attraction effect trial")
			if(correct){
				ctx.fillStyle='green';
				ctx.fillText('CORRECT! Turtle '+jkl_correct.toUpperCase()+ ' is most likely to be in the subspecies.', x_center-.5*ctx.measureText('CORRECT! Turtle '+jkl_correct.toUpperCase() + ' is most likely to be in the subspecies.').width, height*.6);
			}else{
				ctx.fillStyle='red';
				ctx.fillText('INCORRECT. Turtle '+jkl_correct.toUpperCase()+' is most likely to be in the subspecies.', x_center-.5*ctx.measureText('INCORRECT. Turtle '+jkl_correct.toUpperCase() + ' is most likely to be in the subspecies.').width, height*.6);
			}
		}else{
			ctx.fillStyle='black';
			ctx.fillText('Thank you.',x_center-.5*ctx.measureText('Thank you').width,height*.6);
		}
	}else{
		ctx.fillText("Which turtle is most likely to be in the subspecies?", x_center-.5*ctx.measureText("Which turtle is most likely to be in the subspecies?").width, height*.6);
		ctx.fillText("J, K, or L?", x_center-.5*ctx.measureText("J, K, or L?").width, height*.7);
	}
};


function convertToRadian(angle){
	radian = (360-angle)*(Math.PI/180);
    return radian;
};

function convertToAngle(radian){
	angle = Math.round(90-((radian*(180/Math.PI))-270));
	return angle
};
