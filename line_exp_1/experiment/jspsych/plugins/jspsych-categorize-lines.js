///NOTE THIS PLUGIN HAS A CRITICAL BUG///
///line_lens_order is the shuffled stimulus array, but line_lens is what is presented to subjects///


jsPsych.plugins['categorize-lines'] = (function() {

    var plugin = {};

    plugin.info = {
        name: 'categorize-lines',
        description: 'line categorization trials',
        parameters: {
            choices: {
                type: jsPsych.plugins.parameterType.STRING,
                array: true,
                pretty_name: 'Choices',
                default: jsPsych.ALL_KEYS,
                description: 'key choices'
            },
            prompt: {
                type: jsPsych.plugins.parameterType.HTML_STRING,
                pretty_name: 'Prompt',
                default: 'Does this line belong in the category',
                description: 'prompt to give participant',
            },
            line_lens: {
                type: jsPsych.plugins.parameterType.INT,
                pretty_name: 'Line Lengths',
                default: undefined,
                description: 'line length(s) in pixels',
                array: 1,
            },
            line_width: {
                type: jsPsych.plugins.parameterType.INT,
                pretty_name: 'Line Width',
                default: 2,
                description: 'line width in pixels (same for all lines)'
            },
            stim_names: {
                type: jsPsych.plugins.parameterType.STRING,
                pretty_name: 'Stimulus Names',
                default: null,
                array: 1,
                description: 'Names to display below each stimulus'
            },
            give_fb: {
                type: jsPsych.plugins.parameterType.BOOL,
                pretty_name: 'Give Feedback',
                default: 0,
                description: 'Should the subject be give feedback after their response?'
            },
            in_cat: {
                type: jsPsych.plugins.parameterType.STRING,
                pretty_name: 'In Category?',
                default: null,
                description: 'Is the stimulus in the category?' //only applies to learning
            },
            corr_ans: {
                type: jsPsych.plugins.parameterType.STRING,
                pretty_name: 'Correct answer',
                default: null,
                description: 'correct choice' //only applies to transfer trials
            },
            fb_y_corr: {
                type: jsPsych.plugins.parameterType.STRING,
                pretty_name: 'Feedback Yes Correct',
                default: 'You were correct! This line DOES belong in the category.',
                description: 'feedback given to subject if their yes response was correct'
            },
            fb_n_corr: {
                type: jsPsych.plugins.parameterType.STRING,
                pretty_name: 'Feedback No Correct',
                default: 'You were correct! This line does NOT belong in the category.',
                description: 'feedback given to subject if their no response was correct'
            },
            fb_y_incorr: {
                type: jsPsych.plugins.parameterType.STRING,
                pretty_name: 'Feedback Yes Incorrect',
                default: 'You were incorrect. This line does NOT belong in the category.',
                description: 'feedback given to subject if their yes response was correct'
            },
            fb_n_incorr: {
                type: jsPsych.plugins.parameterType.STRING,
                pretty_name: 'Feedback No Incorrect',
                default: 'You were incorrect. This line DOES belong in the category.',
                description: 'Feedback given to subject if their no response was incorrect'
            },
            fb_dur_corr: {
                type: jsPsych.plugins.parameterType.INT,
                pretty_name: 'Feedback Duration if Correct',
                default: 750,
                description: 'duration of feedback if choice was correct'
            },
            fb_dur_incorr: {
                type: jsPsych.plugins.parameterType.INT,
                pretty_name: 'Feedback Duration if incorrect',
                default: 10000,
                description: 'duration of feedback if choice was incorrect'
            },
            dist_btw: {
                type: jsPsych.plugins.parameterType.INT,
                pretty_name: 'Distance Between Stimuli',
                default: 50,
                description: 'On average, how far apart in pixels lines are in space (note: values too high could cause issues). A lot of noise is added to this value.'
            },
            draw_prompt: {
                type: jsPsych.plugins.parameterType.BOOL,
                pretty_name: 'Draw Prompt',
                default: 1,
                description: 'Should the prompt be shown?'
            },
            fix_dur: {
                type: jsPsych.plugins.parameterType.INT,
                pretty_name: 'Fixation Symbol Duration',
                default: 500,
                description: 'Duration of fixation cross in ms'
            },
            post_fix_dur: {
                type: jsPsych.plugins.parameterType.INT,
                pretty_name: 'Post-Fixation Symbol Duration',
                default: 500,
                description: 'Duration of blank screen between fix cross and stimuli'
            }
        }
    }

    plugin.trial = function(display_element, trial) {

        // line lengths of stimuli
        var line_lens = trial.line_lens;

        var line_lens_order = jsPsych.randomization.shuffle(line_lens);

        //number of stimuli
        var n_stim = trial.line_lens.length;
        console.log(trial.line_lens);
        console.log(line_lens_order)
        console.log(trial.line_lens.length);
        console.log(n_stim);

        // width of each line in pixels
        var line_width = trial.line_width;

        // prompt to ask for response
        var prompt = trial.prompt;

        // names of stimuli (e.g,. j,k or l)
        var stim_names = trial.stim_names;

        // is the subject receving feedback?
        var give_fb = trial.give_fb;

        // fb yes correct
        var fb_y_corr = trial.fb_y_corr;

        // fb yes incorrect
        var fb_y_incorr = trial.fb_y_incorr;

        // fb no correct
        var fb_n_corr = trial.fb_n_corr;

        // fb no incorrect
        var fb_n_incorr = trial.fb_n_incorr;

        var choices = trial.choices;
        console.log(choices)

        var in_cat = trial.in_cat;

        var corr_ans = trial.corr_ans;

        // fb durations for correct & incorrect
        var fb_dur_corr = trial.fb_dur_corr;
        var fb_dur_incorr = trial.fb_dur_incorr;

        // is the prompt shown
        var draw_prompt = trial.draw_prompt;

        // avg. dist btw stimuli (noise is added)
        var dist_btw = trial.dist_btw;

        var fix_dur = trial.fix_dur;
        var post_fix_dur = trial.post_fix_dur;

        // screen center
        var x_center = screen.availWidth / 2;
        var y_center = screen.availHeight / 2;

        // canvas dimensions
        var canvas_width = screen.width;
        var canvas_height = screen.height;

        // Stimuli move around the screen. This is done by creating an invisible 'box' and having it jump around
        // box dimensions initialized here
        var box_width = .25 * canvas_width;
        var box_height = 600;

        // Draw imaginary box around stimuli
        var box_top_left = [x_center - (.5 * box_width), y_center - (.5 * box_height)];
        var box_top_right = [x_center + (.5 * box_width), y_center - (.5 * box_height)];
        var box_bottom_right = [x_center + (.5 * box_width), y_center + (.5 * box_height)];
        var box_bottom_left = [x_center - (.5 * box_width), y_center + (.5 * box_height)];

        // how much room to leave for prompt/fb text
        var roomfortext = 100;

        // maximum noise in the x & y axes for box movement
        var xmaxnoise = canvas_width - box_top_right[0];
        var ymaxnoise = canvas_height - box_bottom_right[1] - roomfortext;

        // Function for generating noise in box movement
        // Not really needed anymore
        function get_box_noise(maxnoise) {
            var noise_val = Math.floor(Math.random() * maxnoise);
            if (noise_val < 0) {
                var noise_val = noise_val - (-noise_val);
            }
            var noise_vec = [noise_val, -noise_val]
            return (noise_vec)
        }

        var x_center_new = [(box_top_left[0] + box_top_right[0]) / 2];
        var y_center_new = [(box_bottom_left[1] + box_top_left[1]) / 2];
        var center_new = [x_center_new[0], y_center_new[0]];

        // now noise has to be added to stimuli placement
        var ymax = 100; //max noise that can be added to yaxis line pos. jitter
        var xmax = 50; //max noise that can be added to xaxis line pos. jitter

        // function for generating noise in line placement
        function line_pos_noise(max) {
            var noise = Math.floor(Math.random() * (max));
            var line_noise = jsPsych.randomization.shuffle([noise, -noise])[1];
            return line_noise
        }

        // noise for all 3 lines, even if all 3 lines aren't used
        // not great programming, but doing this inside drawing functions would make stimuli different during feedback
        // currently have all noise removed (5/3/2021)
        var line_noise_x_1 = 0; //line_pos_noise(xmax);
        var line_noise_x_2 = 0; //line_pos_noise(xmax);
        var line_noise_x_3 = 0; //line_pos_noise(xmax);
        var line_noise_y_1 = 0; //line_pos_noise(ymax);
        var line_noise_y_2 = 0; //line_pos_noise(ymax);
        var line_noise_y_3 = 0; //line_pos_noise(ymax);

        var text_loc = [screen.width / 3.35, canvas_height - 25];


        // init response variable
        var response = {
            rt: null,
            key: null
        };

        // draw fixation cross
        function draw_fix_cross() {
            var html2 = '<canvas id="myCanvas2" width=' + canvas_width + ' height=' + canvas_height + ' ;"></canvas>';
            display_element.innerHTML = html2;
            var c1 = document.getElementById("myCanvas2");
            var ctx = c1.getContext("2d");
            // draw fixation cross
            ctx.lineWidth = line_width;
            ctx.beginPath();
            ctx.moveTo(center_new[0] - 25, center_new[1]);
            ctx.lineTo(center_new[0] + 25, center_new[1]);
            ctx.stroke();
            ctx.moveTo(center_new[0], center_new[1] - 25);
            ctx.lineTo(center_new[0], center_new[1] + 25);
            ctx.stroke();
            ctx.fill();

        }

        // clear screen following fixation cross
        function clear_screen(ctx) {
            var html3 = '<canvas id="myCanvas2" width=' + canvas_width + ' height=' + canvas_height + ' ;"></canvas>';
            display_element.innerHTML = html3;
            var c1 = document.getElementById("myCanvas2");
            var ctx = c1.getContext("2d");
            jsPsych.pluginAPI.setTimeout(function() {
                ctx.clearRect(0, 0, ctx.width, ctx.height);
            }, 500);
        }

        // draw a single line
        function draw_line_1(givefeedbacktext = 0, fbtxt = '', draw_prompt = 1, fb_col = null) { //defaults for trials
            let line_length_1 = trial.line_lens[0];
            //canvas for drawing stimulus
            var html2 = '<canvas id="myCanvas2" width=' + canvas_width + ' height=' + canvas_height + ' ;"></canvas>';
            display_element.innerHTML = html2;
            var c1 = document.getElementById("myCanvas2");
            var ctx = c1.getContext("2d");

            // getting line begninning and end
            var line1_begin = [center_new[0] + line_noise_x_1, (center_new[1] + (line_length_1 / 2)) + line_noise_y_1];
            var line1_end = [center_new[0] + line_noise_x_1, (center_new[1] - (line_length_1 / 2)) + line_noise_y_1];
            let line_1_text = [line1_begin[0] - 5, line1_begin + 40];

            // drawing line
            ctx.lineWidth = line_width;
            ctx.beginPath();
            ctx.moveTo(line1_begin[0], line1_begin[1]);
            ctx.lineTo(line1_end[0], line1_end[1]);
            ctx.stroke();
            ctx.font = '24px sans-serif';
            //drawing text
            ctx.fillText(stim_names, line_1_text[0], line_1_text[1]);

            //drawing feedback text
            if (givefeedbacktext) {
                ctx.fillStyle = fb_col;
                ctx.fillText(fb_txt, text_loc[0], text_loc[1]);
            }
            //drawing prompt
            if (draw_prompt) {
                ctx.fillText(prompt, text_loc[0], text_loc[1]);
            }
        }

        // draw two lines
        function draw_line_2(givefeedbacktext = 0, fbtxt = '', draw_prompt = 1) { //defaults for trials
            let line_lengths_2 = trial.line_lens;
            var html3 = '<canvas id="myCanvas2" width=' + canvas_width + ' height=' + canvas_height + ' ;"></canvas>';
            display_element.innerHTML = html3;
            var c1 = document.getElementById("myCanvas2");
            var ctx = c1.getContext("2d");

            var line1_begin = [center_new[0] - (.5 * dist_btw) + line_noise_x_1, (center_new[1] + (line_lengths_2[0] / 2)) + line_noise_y_1];
            var line1_end = [center_new[0] - (.5 * dist_btw) + line_noise_x_1, (center_new[1] - (line_lengths_2[0] / 2)) + line_noise_y_1];
            var line2_begin = [center_new[0] + (.5 * dist_btw) + line_noise_x_2, (center_new[1] + (line_lengths_2[1] / 2)) + line_noise_y_2];
            var line2_end = [center_new[0] + (.5 * dist_btw) + line_noise_x_2, (center_new[1] - (line_lengths_2[1] / 2)) + line_noise_y_2];
            var mintxt = Math.max(line1_begin[1], line2_begin[1]) + 40;
            let line_1_text = [line1_begin[0] - 5, mintxt];
            let line_2_text = [line2_begin[0] - 5, mintxt];

            ctx.lineWidth = line_width;
            ctx.beginPath();
            ctx.moveTo(line1_begin[0], line1_begin[1]);
            ctx.lineTo(line1_end[0], line1_end[1]);
            ctx.stroke();
            ctx.font = '24px sans-serif';
            ctx.fillText(stim_names[0], line_1_text[0], line_1_text[1]);

            ctx.lineWidth = line_width;
            ctx.beginPath();
            ctx.moveTo(line2_begin[0], line2_begin[1]);
            ctx.lineTo(line2_end[0], line2_end[1]);
            ctx.stroke();
            ctx.fillText(stim_names[1], line_2_text[0], line_2_text[1]);
            if (givefeedbacktext) {
                ctx.fillText(fb_txt, text_loc[0], text_loc[1]);
            }
            if (draw_prompt) {
                ctx.fillText(prompt, text_loc[0], text_loc[1]);
            }
        }

        // draw 3 lines
        function draw_line_3(givefeedbacktext = 0, fb_txt = '', draw_prompt = 1) { //defaults for trials
            let line_lengths_3 = trial.line_lens;
            var html3 = '<canvas id="myCanvas2" width=' + canvas_width + ' height=' + canvas_height + ' ;"></canvas>';
            display_element.innerHTML = html3;
            var c1 = document.getElementById("myCanvas2");
            var ctx = c1.getContext("2d");
            var line1_begin = [center_new[0] - (.5 * dist_btw) + line_noise_x_1, (center_new[1] + (line_lengths_3[0] / 2)) + line_noise_y_1];
            var line1_end = [center_new[0] - (.5 * dist_btw) + line_noise_x_1, (center_new[1] - (line_lengths_3[0] / 2)) + line_noise_y_1];
            var line2_begin = [center_new[0] + line_noise_x_2, (center_new[1] + (line_lengths_3[1] / 2)) + line_noise_y_2];
            var line2_end = [center_new[0] + line_noise_x_2, (center_new[1] - (line_lengths_3[1] / 2)) + line_noise_y_2];
            var line3_begin = [center_new[0] + (.5 * dist_btw) + line_noise_x_3, (center_new[1] + (line_lengths_3[2] / 2)) + line_noise_y_3];
            var line3_end = [center_new[0] + (.5 * dist_btw) + line_noise_x_3, (center_new[1] - (line_lengths_3[2] / 2)) + line_noise_y_3];
            var mintxt = Math.max(line1_begin[1], line2_begin[1], line3_begin[1]) + 40;
            let line_1_text = [line1_begin[0] - 5, mintxt];
            let line_2_text = [line2_begin[0] - 5, mintxt];
            let line_3_text = [line3_begin[0] - 5, mintxt];

            ctx.lineWidth = line_width;
            ctx.beginPath();
            ctx.moveTo(line1_begin[0], line1_begin[1]);
            ctx.lineTo(line1_end[0], line1_end[1]);
            ctx.stroke();
            ctx.font = '24px sans-serif';
            ctx.fillText(stim_names[0], line_1_text[0], line_1_text[1]);

            ctx.lineWidth = line_width;
            ctx.beginPath();
            ctx.moveTo(line2_begin[0], line2_begin[1]);
            ctx.lineTo(line2_end[0], line2_end[1]);
            ctx.stroke();
            ctx.fillText(stim_names[1], line_2_text[0], line_2_text[1]);

            ctx.lineWidth = line_width;
            ctx.beginPath();
            ctx.moveTo(line3_begin[0], line3_begin[1]);
            ctx.lineTo(line3_end[0], line3_end[1]);
            ctx.stroke();
            ctx.fillText(stim_names[2], line_3_text[0], line_3_text[1]);
            if (givefeedbacktext) {
                ctx.fillText(fb_txt, text_loc[0], text_loc[1]);
            }
            if (draw_prompt) {
                ctx.fillText(prompt, text_loc[0], text_loc[1]);
            }
        }

        function begin(){
          draw_fix_cross();
          jsPsych.pluginAPI.setTimeout(blank_screen, fix_dur);
        }

        function blank_screen(){
        	clear_screen();
        	jsPsych.pluginAPI.setTimeout(draw_lines, post_fix_dur);
        }

        function draw_lines(){
        	if(n_stim == 1){
        		draw_line_1();
        	}
        	if(n_stim == 2){
        		draw_line_2();
        	}
        	if(n_stim == 3){
        		draw_line_3();
        	}
          var keyboard_listener = jsPsych.pluginAPI.getKeyboardResponse({
           callback_function: after_response,
           valid_responses: trial.choices,
           rt_method: 'performance',
           persist: false,
           allow_held_key: false,
          });
          return keyboard_listener
        }

        // Single stimulus presentation - this is for learning trials
        // Where subjects must answer y/n to is this line in the category
        function after_response(info) {
            if (response.key == null) {
                response = info;
            }
            if (give_fb) {
                console.log(in_cat);
                console.log(response.key);
                if (n_stim == 1) {
                    if (response.key == trial.choices[0]) { //if they say yes
                        //for learning trials
                        console.log('yes response');
                        response.yn_choice = "YES";
                        if (in_cat == 'in') { //if the stimulus IS in the category and participant said it was
                            var fb_str = fb_y_corr;
                            var fb_dur = fb_dur_corr;
                            response.correct = 1;
                            response.fb_in_cat = in_cat;
                            draw_line_1(givefeedbacktext = 1, fb_txt = fb_str, draw_prompt = 0, fb_col = 'green');
                            jsPsych.pluginAPI.setTimeout(function() {
                                end_trial(response);
                            }, fb_dur);
                        } else if (in_cat == 'out') { //if the stimulus is NOT in the category and participant said it was
                            var fb_str = fb_y_incorr;
                            var fb_dur = fb_dur_incorr;
                            response.correct = 0;
                            response.fb_in_cat = in_cat;
                            draw_line_1(givefeedbacktext = 1, fb_txt = fb_str, draw_prompt = 0, fb_col = 'red');
                            jsPsych.pluginAPI.setTimeout(function() {
                                end_trial(response);
                            }, fb_dur);
                        }
                    } else if (response.key == trial.choices[1]) { //if they say no
                        console.log('no response')
                        response.yn_choice = "NO";
                        if (in_cat == 'in') { //if the stimulus IS in the category and participant said it wasn't
                            var fb_str = fb_n_incorr;
                            var fb_dur = fb_dur_incorr;
                            response.correct = 0;
                            response.fb_in_cat = in_cat;
                            draw_line_1(givefeedbacktext = 1, fb_txt = fb_str, draw_prompt = 0, fb_col = 'red');
                            jsPsych.pluginAPI.setTimeout(function() {
                                end_trial(response);
                            }, fb_dur);
                        } else if (in_cat == 'out') { //if the stimulus is NOT in the category and participant said it wasn't
                            var fb_str = fb_n_corr;
                            var fb_dur = fb_dur_corr;
                            response.correct = 1;
                            response.fb_in_cat = in_cat;
                            draw_line_1(givefeedbacktext = 1, fb_txt = fb_str, draw_prompt = 0, fb_col = 'green');
                            jsPsych.pluginAPI.setTimeout(function() {
                                end_trial(response);
                            }, fb_dur);
                        }
                    }
                } else if (n_stim == 2) {
                    if (response.key == corr_ans) {
                        response.correct = 1;
                        var fb_str = "You are correct! " + corr_ans.toUpperCase() + " is the most likely to belong to the category.";
                        response.fb_in_cat = null;
                        draw_line_2(givefeedbacktext = 1, fb_txt = fb_str, draw_prompt = 0);
                        jsPsych.pluginAPI.setTimeout(function() {
                            end_trial(response);
                        }, fb_dur_corr);
                    } else if (response.key != corr_ans) {
                        response.correct = 0;
                        var fb_str = "You are Incorrect. " + corr_ans.toUpperCase() + " is the most likely to belong to the category.";
                        response.fb_in_cat = null;
                        draw_line_2(givefeedbacktext = 1, fb_txt = fb_str, draw_prompt = 0);
                        jsPsych.pluginAPI.setTimeout(function() {
                            end_trial(response);
                        }, fb_dur_incorr);
                    } else if (n_stim == 3) {
                        console.log(response);
                        if (response.key == corr_ans) {
                            response.correct = 1;
                            var fb_str = "You are correct! " + corr_ans.toUpperCase() + " is the most likely to belong to the category.";
                            response.fb_in_cat = null;
                            draw_line_3(givefeedbacktext = 1, fb_txt = fb_str, draw_prompt = 0);
                            jsPsych.pluginAPI.setTimeout(function() {
                                end_trial(response);
                            }, fb_dur_corr);
                        } else if (response.key != corr_ans) {
                            response.correct = 0;
                            var fb_str = "You are Incorrect. " + corr_ans.toUpperCase() + " is the most likely to belong to the category.";
                            response.fb_in_cat = null;
                            draw_line_3(givefeedbacktext = 1, fb_txt = fb_str, draw_prompt = 0);
                            jsPsych.pluginAPI.setTimeout(function() {
                                end_trial(response);
                            }, fb_dur_incorr);
                        }
                    }
                }
            } else if (!give_fb) {
                response.fb_in_cat = null;
                if (n_stim == 2) {
                    if (response.key == choices[0]) {
                        console.log('j was chosen')
                        response.choice = line_lens_order[0];
                        if (response.choice == corr_ans) {
                            console.log("You were correct")
                            response.correct = 1;
                        } else if (response.choice != corr_ans) {
                            console.log("You were not correct")
                            response.correct = 0;
                        }
                    } else if (response.key == choices[1]) {
                        console.log('k was chosen')
                        response.choice = line_lens_order[1];
                        if (response.choice == corr_ans) {
                            console.log("You were correct")
                            response.correct = 1;
                        } else if (response.choice != corr_ans) {
                            console.log("You were not correct")
                            response.correct = 0;
                        }
                    }
                    jsPsych.pluginAPI.setTimeout(function() {
                        end_trial(response);
                    }, 50);
                } else if (n_stim == 3) {
                    if (response.key == choices[0]) {
                        console.log('j was chosen')
                        response.choice = line_lens_order[0];
                        console.log(response.choice)
                        if (response.choice == corr_ans) {
                            console.log("You were correct")
                            response.correct = 1;
                        } else if (response.choice != corr_ans) {
                            console.log("You were not correct")
                            response.correct = 0;
                        }
                    } else if (response.key == choices[1]) {
                        console.log('k was chosen')
                        response.choice = line_lens_order[1];
                        if (response.choice == corr_ans) {
                            console.log("You were correct")
                            response.correct = 1;
                        } else if (response.choice != corr_ans) {
                            console.log("You were not correct")
                            response.correct = 0;
                        }
                    } else if (response.key == choices[2]) {
                        console.log(response.key)
                        console.log('l was chosen')
                        response.choice = line_lens_order[2];
                        console.log(response.choice)
                        if (response.choice == corr_ans) {
                            console.log("You were correct")
                            response.correct = 1;
                        } else if (response.choice != corr_ans) {
                            console.log("You were not correct")
                            response.correct = 0;
                        }
                    }
                }
                jsPsych.pluginAPI.setTimeout(function() {
                    end_trial(response);
                }, 50);
            }
        }

        console.log(trial.choices);


        // function to end trial when it is time
        function end_trial(response) {
            // kill any remaining setTimeout handlers
            jsPsych.pluginAPI.clearAllTimeouts();

            // kill keyboard listeners
            if (typeof keyboard_listener !== 'undefined') {
                jsPsych.pluginAPI.cancelKeyboardResponse(keyboard_listener);
            }

            // data saving
            var trial_data = {
                "line_lens": trial.line_lens,
                "line_lens_order": line_lens_order,
                "choices": trial.choices,
                "stim_names": trial.stim_names,
                "corr_ans": trial.corr_ans,
                "give_fb": trial.give_fb,
                "in_cat": trial.in_cat,
                "rt": response.rt,
                "key_choice": response.key,
                "yn_choice": response.yn_choice,
                "choice": response.choice,
                "correct": response.correct,
                "fb_in_cat": response.fb_in_cat,
                "stim_1_in_order": line_lens_order[1],
                "stim_2_in_order": line_lens_order[2],
                "stim_3_in_order": line_lens_order[3]
            }
            console.log(response);
            // clear the display
            display_element.innerHTML = '';

            // move on to the next trial
            jsPsych.finishTrial(trial_data);
        }

        // init
        begin();
    };
    return plugin;
})();