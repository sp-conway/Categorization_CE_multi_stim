jsPsych.plugins['categorize-lines-2'] = (function() {

    var plugin = {};

    plugin.info = {
        name: 'categorize-lines-2',
        description: 'line categorization trials',
        parameters: {
            stim_vals: {
                type: jsPsych.plugins.parameterType.FLOAT,
                array: true,
                pretty_name: "Stimulus Values",
                default: null
            },
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
            trial_choice_set: {
                type: jsPsych.plugins.parameterType.STRING,
                pretty_name: "Trial Choice Set",
                default: null,
                description: "Choice Set for this trial (unitary v binary v trinary)"
            },
            scale_stim: {
                type: jsPsych.plugins.parameterType.BOOL,
                pretty_name: "Scale stimuli?",
                default: false,
                description: "Should stimuli be scaled using psy2phy()"
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
                default: false,
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
                array: true,
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
                default: 1000,
                description: 'duration of feedback if choice was correct'
            },
            fb_dur_incorr: {
                type: jsPsych.plugins.parameterType.INT,
                pretty_name: 'Feedback Duration if incorrect',
                default: 3000,
                description: 'duration of feedback if choice was incorrect'
            },
            dist_btw: {
                type: jsPsych.plugins.parameterType.INT,
                pretty_name: 'Distance Between Stimuli',
                default: 50,
                description: 'distance between each line when multiple lines are on screen.'
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
            },
            target_phys: {
                type: jsPsych.plugins.parameterType.INT,
                pretty_name: "Target Line Length",
                default: null
            },
            competitor_phys: {
                type: jsPsych.plugins.parameterType.INT,
                pretty_name: "Competitor Line Length",
                default: null
            },
            decoy_phys: {
                type: jsPsych.plugins.parameterType.INT,
                pretty_name: "Decoy Line Length",
                default: null
            },
            transfer_trial_type: {
                type: jsPsych.plugins.parameterType.STRING,
                default: "NA"
            }
        }
    }

    plugin.trial = function(display_element, trial) {

        // some functions for going from psy to phys stim (or vice versa)
        function psy2phy(psy) {
            var phy = 150 * (1.05 ^ psy)
            return phy
        }

        function log(b, n) {
            return Math.log(n) / Math.log(b)
        }

        function phy2psy(phy) {
            var psy = log(b = 1.05, n = phy / 150)
            return (psy)
        }

        // init line lens vector
        var line_lens = [];

        // log raw stim values
        console.log('stim_vals=', trial.stim_vals)

        // scale stimuli if applicable
        if (trial.scale_stim) {
            for (i = 0; i <= trial.stim_vals.length; i++) {
                var line_lens = [line_lens, psy2phy(trial.stim_vals[i])]
                console.log('line_lens=', [i], line_lens)
            }
        } else { // otherwise use "raw" input values
            var line_lens = trial.stim_vals
            console.log('line_lens=', line_lens)
        }

        // shuffle the line lengths
        var line_lens_order = jsPsych.randomization.shuffle(line_lens);

        // get number of stimuli
        var n_stim = trial.stim_vals.length;
        console.log('n_stim = ', n_stim)

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

        // choice options
        var choices = trial.choices;

        // is the option in the category (only applies for unitary trials)
        var in_cat = trial.in_cat;

        // target/competitor/decoy lengths
        var target_phys = trial.target_phys
        var competitor_phys = trial.competitor_phys
        var decoy_phys = trial.decoy_phys
        var transfer_trial_type = jsPsych.timelineVariable('transfer_trial_type')

        if (n_stim == 1) {
            console.log("in_cat = ", in_cat);
        }

        if (transfer_trial_type == "attraction") {
            console.log("This is an attraction effect trial")
            if (n_stim == 2) {
                console.log('target = ', target_phys)
                console.log('competitor = ', competitor_phys)
            } else {
                console.log('target = ', target_phys)
                console.log('competitor = ', competitor_phys)
                console.log('decoy = ', decoy_phys)
            }
        } else {
            console.log("transfer_trial_type= ", transfer_trial_type)
        }

        // what is/are the correct answer(s)
        var corr_ans = trial.corr_ans;
        if (corr_ans == 'sampled_1') {
            var corr_ans = [trial.stim_vals[0]]
        }

        console.log("line_lens= ", line_lens)
        console.log("line_lens_order= ", line_lens_order)
        console.log("correct answer = ", corr_ans)


        // fb durations for correct & incorrect
        var fb_dur_corr = trial.fb_dur_corr;
        var fb_dur_incorr = trial.fb_dur_incorr;

        // is the prompt shown
        var draw_prompt = trial.draw_prompt;

        // dist btw stimuli
        var dist_btw = trial.dist_btw;

        // duration to show fixation cross
        var fix_dur = trial.fix_dur;

        // duration between fixation cross and stimulus
        var post_fix_dur = trial.post_fix_dur;

        // box center
        var x_center = screen.availWidth / 2;
        var y_center = screen.availHeight * .75;

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

        // find new center
        var x_center_new = (box_top_left[0] + box_top_right[0]) / 2;
        var y_center_new = (box_bottom_left[1] + box_top_left[1]) / 2;
        var center_new = [x_center_new, y_center_new];

        // text location
        var text_loc = [x_center_new - .75 * box_width, box_bottom_left[1] - .25 * box_height];

        // init response object
        var response = {
            rt: null,
            key: null
        };

        // // draw fixation cross
        function draw_fix_cross() {
            var html2 = '<canvas id="myCanvas2" width=' + canvas_width + ' height=' + canvas_height + ' ;"></canvas>';
            display_element.innerHTML = html2;
            var c1 = document.getElementById("myCanvas2");
            var ctx = c1.getContext("2d");
            // draw fixation cross
            ctx.lineWidth = line_width;
            ctx.beginPath();
            ctx.moveTo(center_new[0] - 25, screen.availHeight * .5);
            ctx.lineTo(center_new[0] + 25, screen.availHeight * .5);
            ctx.stroke();
            ctx.moveTo(center_new[0], screen.availHeight * .5 - 25);
            ctx.lineTo(center_new[0], screen.availHeight * .5 + 25);
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
        function draw_line_1(give_fb_txt = 0, fbtxt = '', draw_prompt = 1, fb_col = null) { //defaults for trials
            //canvas for drawing stimulus
            var html2 = '<canvas id="myCanvas2" width=' + canvas_width + ' height=' + canvas_height + ' ;"></canvas>';
            display_element.innerHTML = html2;
            var c1 = document.getElementById("myCanvas2");
            var ctx = c1.getContext("2d");

            // getting line begninning and end
            var line1_begin = [center_new[0], center_new[1]];
            var line1_end = [center_new[0], center_new[1] - line_lens_order[0]];
            var line_1_text = [line1_begin[0] - 5, line1_begin + 40];

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
            if (give_fb_txt) {
                ctx.fillStyle = fb_col;
                ctx.fillText(fb_txt, text_loc[0], text_loc[1]);
            }
            //drawing prompt
            if (draw_prompt) {
                ctx.fillText(prompt, text_loc[0], text_loc[1]);
            }
        }

        // draw two lines
        function draw_line_2(give_fb_txt = 0, fbtxt = '', draw_prompt = 1) { //defaults for trials
            //console.log("line_lens_order = ", line_lens_order)
            var html3 = '<canvas id="myCanvas2" width=' + canvas_width + ' height=' + canvas_height + ' ;"></canvas>';
            display_element.innerHTML = html3;
            var c1 = document.getElementById("myCanvas2");
            var ctx = c1.getContext("2d");
            var line1_begin = [center_new[0] - (.5 * dist_btw), center_new[1]];
            //console.log('line1_begin = ',line1_begin)
            var line1_end = [center_new[0] - (.5 * dist_btw), center_new[1] - line_lens_order[0]];
            //console.log('line1_end = ',line1_end)
            var line2_begin = [center_new[0] + (.5 * dist_btw), center_new[1]];
            //console.log('line2_begin = ',line2_begin)
            var line2_end = [center_new[0] + (.5 * dist_btw), center_new[1] - line_lens_order[1]];
            //console.log('line2_begin = ',line2_begin)
            var mintxt = Math.max(line1_begin[1], line2_begin[1]) + 40;
            var line_1_text = [line1_begin[0] - 5, mintxt];
            var line_2_text = [line2_begin[0] - 5, mintxt];

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
            ctx.font = '24px sans-serif';
            ctx.fillText(stim_names[1], line_2_text[0], line_2_text[1]);
            if (give_fb_txt) {
                ctx.fillStyle = fb_col;
                ctx.fillText(fb_txt, line1_begin[0], text_loc[1]);
            }
            if (draw_prompt) {
                ctx.fillText(prompt, text_loc[0], text_loc[1]);
            }
        }

        // draw 3 lines
        function draw_line_3(give_fb_txt = 0, fb_txt = '', draw_prompt = 1) { //defaults for trials
            //console.log("line_lens_order=",line_lens_order)
            var html3 = '<canvas id="myCanvas2" width=' + canvas_width + ' height=' + canvas_height + ' ;"></canvas>';
            display_element.innerHTML = html3;
            var c1 = document.getElementById("myCanvas2");
            var ctx = c1.getContext("2d");
            var line1_begin = [center_new[0] - (.5 * dist_btw), center_new[1]];
            var line1_end = [center_new[0] - (.5 * dist_btw), center_new[1] - line_lens_order[0]];
            var line2_begin = [center_new[0], center_new[1]];
            var line2_end = [center_new[0], center_new[1] - line_lens_order[1]];
            var line3_begin = [center_new[0] + (.5 * dist_btw), center_new[1]];
            var line3_end = [center_new[0] + (.5 * dist_btw), center_new[1] - line_lens_order[2]];
            var mintxt = Math.max(line1_begin[1], line2_begin[1], line3_begin[1]) + 40;
            var line_1_text = [line1_begin[0] - 5, mintxt];
            var line_2_text = [line2_begin[0] - 5, mintxt];
            var line_3_text = [line3_begin[0] - 5, mintxt];

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
            ctx.font = '24px sans-serif';
            ctx.fillText(stim_names[1], line_2_text[0], line_2_text[1]);

            ctx.lineWidth = line_width;
            ctx.beginPath();
            ctx.moveTo(line3_begin[0], line3_begin[1]);
            ctx.lineTo(line3_end[0], line3_end[1]);
            ctx.stroke();
            ctx.font = '24px sans-serif';
            ctx.fillText(stim_names[2], line_3_text[0], line_3_text[1]);
            if (give_fb_txt) {
                ctx.fillStyle = fb_col;
                ctx.fillText(fb_txt, line1_begin[0], text_loc[1]);
            }
            if (draw_prompt) {
                ctx.fillText(prompt, text_loc[0], text_loc[1]);
            }
        }

        function begin() {
            draw_fix_cross();
            jsPsych.pluginAPI.setTimeout(blank_screen, fix_dur);
        }

        function blank_screen() {
            clear_screen();
            jsPsych.pluginAPI.setTimeout(draw_lines, post_fix_dur);
        }

        function draw_lines() {
            switch (n_stim) {
                case 1:
                    console.log('drawing 1 line')
                    draw_line_1()
                    break;
                case 2:
                    console.log('drawing 2 lines')
                    draw_line_2()
                    break;
                case 3:
                    console.log('drawing 3 lines')
                    draw_line_3()
                    break;
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
                        response.choice = "YES";
                        response.choice_psy = 'NA';
                        if (in_cat == 'in') { //if the stimulus IS in the category and participant said it was
                            var fb_str = fb_y_corr;
                            var fb_dur = fb_dur_corr;
                            response.correct = 1;
                            response.fb_in_cat = in_cat;
                            draw_line_1(givefeedbacktext = true, fb_txt = fb_str, draw_prompt = 0, fb_col = 'green');
                            jsPsych.pluginAPI.setTimeout(function() {
                                end_trial(response);
                            }, fb_dur);
                        } else if (in_cat == 'out') { //if the stimulus is NOT in the category and participant said it was
                            var fb_str = fb_y_incorr;
                            var fb_dur = fb_dur_incorr;
                            response.correct = 0;
                            response.fb_in_cat = in_cat;
                            draw_line_1(givefeedbacktext = true, fb_txt = fb_str, draw_prompt = 0, fb_col = 'red');
                            jsPsych.pluginAPI.setTimeout(function() {
                                end_trial(response);
                            }, fb_dur);
                        }
                    } else if (response.key == trial.choices[1]) { //if they say no
                        console.log('no response')
                        response.choice = "NO";
                        response.choice_psy = 'NA';
                        if (in_cat == 'in') { //if the stimulus IS in the category and participant said it wasn't
                            var fb_str = fb_n_incorr;
                            var fb_dur = fb_dur_incorr;
                            response.correct = 0;
                            response.fb_in_cat = in_cat;
                            draw_line_1(givefeedbacktext = true, fb_txt = fb_str, draw_prompt = 0, fb_col = 'red');
                            jsPsych.pluginAPI.setTimeout(function() {
                                end_trial(response);
                            }, fb_dur);
                        } else if (in_cat == 'out') { //if the stimulus is NOT in the category and participant said it wasn't
                            var fb_str = fb_n_corr;
                            var fb_dur = fb_dur_corr;
                            response.correct = 1;
                            response.fb_in_cat = in_cat;
                            draw_line_1(givefeedbacktext = true, fb_txt = fb_str, draw_prompt = 0, fb_col = 'green');
                            jsPsych.pluginAPI.setTimeout(function() {
                                end_trial(response);
                            }, fb_dur);
                        }
                    }
                } else { // in the case of multiple stimuli
                    switch (response.key) {
                        case trial.choices[0]:
                            response.choice = line_lens_order[0];
                            response.choice_psy = phy2psy(line_lens_order[0])
                            break;
                        case trial.choices[1]:
                            response.choice = line_lens_order[1];
                            response.choice_psy = phy2psy(line_lens_order[1])
                            break;
                        case trial.choices[2]:
                            response.choice = line_lens_order[2];
                            response.choice_psy = phy2psy(line_lens_order[2])
                            break;
                    }
                    if (Math.abs(response.choice - corr_ans[0]) <= .005) { // allowing some room for error going from R to js
                        response.correct = 1
                    } else if (Math.abs(response.choice - corr_ans[1]) <= .005) {
                        response.correct = 1
                    } else {
                        response.correct = 0
                    }
                    console.log("key choice = ", response.key)
                    console.log("line choice = ", response.choice)
                    console.log("psy choice = ", response.choice_psy)
                    console.log("correct = ", response.correct)
                }
                if (n_stim == 2) {
                    if (response.correct) {
                        draw_line_2(give_fb_txt = true, fb_txt = "That is correct!", draw_prompt = 0, fb_col = "green");
                        jsPsych.pluginAPI.setTimeout(function() {
                            end_trial(response);
                        }, fb_dur_corr);
                    } else {
                        draw_line_2(give_fb_txt = true, fb_txt = "That is incorrect.", draw_prompt = 0, fb_col = "red");
                        jsPsych.pluginAPI.setTimeout(function() {
                            end_trial(response);
                        }, fb_dur_incorr);
                    }
                } else if (n_stim == 3) {
                    if (response.correct) {
                        draw_line_3(give_fb_txt = true, fb_txt = "That is correct!", draw_prompt = 0, fb_col = "green");
                        jsPsych.pluginAPI.setTimeout(function() {
                            end_trial(response);
                        }, fb_dur_corr);
                    } else {
                        draw_line_3(give_fb_txt = true, fb_txt = "That is incorrect.", draw_prompt = 0, fb_col = "red");
                        jsPsych.pluginAPI.setTimeout(function() {
                            end_trial(response);
                        }, fb_dur_incorr);
                    }
                }
            } else {
                switch (response.key) {
                    case trial.choices[0]:
                        response.choice = line_lens_order[0];
                        response.choice_psy = phy2psy(line_lens_order[0])
                        if (Math.abs(response.choice - corr_ans[0]) <= .005) { // allowing some room for error going from R to js
                            response.correct = 1
                        } else if (Math.abs(response.choice - corr_ans[1]) <= .005) {
                            response.correct = 1
                        } else {
                            response.correct = 0
                        }
                        console.log("key choice = ", response.key)
                        console.log("line choice = ", response.choice)
                        console.log("psy choice = ", response.choice_psy)
                        console.log("correct = ", response.correct)
                        break;
                    case trial.choices[1]:
                        response.choice = line_lens_order[1];
                        response.choice_psy = phy2psy(line_lens_order[1])
                        if (Math.abs(response.choice - corr_ans[0]) <= .005) { // allowing some room for error going from R to js
                            response.correct = 1
                        } else if (Math.abs(response.choice - corr_ans[1]) <= .005) {
                            response.correct = 1
                        } else {
                            response.correct = 0
                        }
                        console.log("key choice = ", response.key)
                        console.log("line choice = ", response.choice)
                        console.log("psy choice = ", response.choice_psy)
                        console.log("correct = ", response.correct)
                        break;
                    case trial.choices[2]:
                        response.choice = line_lens_order[2];
                        response.choice_psy = phy2psy(line_lens_order[2])
                        if (Math.abs(response.choice - corr_ans[0]) <= .005) { // allowing some room for error going from R to js
                            response.correct = 1
                        } else if (Math.abs(response.choice - corr_ans[1]) <= .005) {
                            response.correct = 1
                        } else {
                            response.correct = 0
                        }
                        console.log("key choice = ", response.key)
                        console.log("line choice = ", response.choice)
                        console.log("psy choice = ", response.choice_psy)
                        console.log("correct = ", response.correct)
                        break;
                }
            }
        }

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
                "stim_vals": trial.stim_vals,
                "line_lens_order": line_lens_order,
                "stim_vals_order_psy": [phy2psy(line_lens_order[0]), phy2psy(line_lens_order[1]), phy2psy(line_lens_order[2])],
                "choices": trial.choices,
                "stim_names": trial.stim_names,
                "corr_ans": trial.corr_ans,
                "give_fb": trial.give_fb,
                "in_cat": trial.in_cat,
                "rt": response.rt,
                "key_choice": response.key,
                "choice": response.choice,
                "choice_psy": response.choice_psy,
                "correct": response.correct,
                "fb_in_cat": response.fb_in_cat,
                "line_1_in_order": line_lens_order[0],
                "line_2_in_order": line_lens_order[1],
                "line_3_in_order": line_lens_order[2],
                "psy_1_in_order": phy2psy(line_lens_order[0]),
                "psy_2_in_order": phy2psy(line_lens_order[1]),
                "psy_3_in_order": phy2psy(line_lens_order[2]),
            }
            console.log(response);
            // clear the display
            display_element.innerHTML = '';

            // move on to the next trial
            jsPsych.finishTrial(trial_data);
        }

        // init
        begin();
    }
    return plugin;
})();


// old code
// if (response.key == corr_ans) {
//     response.correct = 1;
//     var fb_str = "You are correct! " + corr_ans.toUpperCase() + " is the most likely to belong to the category.";
//     response.fb_in_cat = null;
//     draw_line_2(give_fb_txt = 1, fb_txt = fb_str, draw_prompt = 0);
//     jsPsych.pluginAPI.setTimeout(function() {
//         end_trial(response);
//     }, fb_dur_corr);
// } else if (response.key != corr_ans) {
//     response.correct = 0;
//     var fb_str = "You are Incorrect. " + corr_ans.toUpperCase() + " is the most likely to belong to the category.";
//     response.fb_in_cat = null;
//     draw_line_2(give_fb_txt = 1, fb_txt = fb_str, draw_prompt = 0);
//     jsPsych.pluginAPI.setTimeout(function() {
//         end_trial(response);
//     }, fb_dur_incorr);
// } else if (n_stim == 3) {
//     console.log(response);
//     if (response.key == corr_ans) {
//         response.correct = 1;
//         var fb_str = "You are correct! " + corr_ans.toUpperCase() + " is the most likely to belong to the category.";
//         response.fb_in_cat = null;
//         draw_line_3(give_fb_txt = 1, fb_txt = fb_str, draw_prompt = 0);
//         jsPsych.pluginAPI.setTimeout(function() {
//             end_trial(response);
//         }, fb_dur_corr);
//     } else if (response.key != corr_ans) {
//         response.correct = 0;
//         var fb_str = "You are Incorrect. " + corr_ans.toUpperCase() + " is the most likely to belong to the category.";
//         response.fb_in_cat = null;
//         draw_line_3(give_fb_txt = 1, fb_txt = fb_str, draw_prompt = 0);
//         jsPsych.pluginAPI.setTimeout(function() {
//             end_trial(response);
//         }, fb_dur_incorr);
//     }
// }
// }
// } else if (!give_fb) {
// response.fb_in_cat = null;
// if (n_stim == 2) {
// if (response.key == choices[0]) {
//     console.log('j was chosen')
//     response.choice = line_lens_order[0];
//     if (response.choice == corr_ans) {
//         console.log("You were correct")
//         response.correct = 1;
//     } else if (response.choice != corr_ans) {
//         console.log("You were not correct")
//         response.correct = 0;
//     }
// } else if (response.key == choices[1]) {
//     console.log('k was chosen')
//     response.choice = line_lens_order[1];
//     if (response.choice == corr_ans) {
//         console.log("You were correct")
//         response.correct = 1;
//     } else if (response.choice != corr_ans) {
//         console.log("You were not correct")
//         response.correct = 0;
//     }
// }
// jsPsych.pluginAPI.setTimeout(function() {
//     end_trial(response);
// }, 50);
// } else if (n_stim == 3) {
// if (response.key == choices[0]) {
//     console.log('j was chosen')
//     response.choice = line_lens_order[0];
//     console.log(response.choice)
//     if (response.choice == corr_ans) {
//         console.log("You were correct")
//         response.correct = 1;
//     } else if (response.choice != corr_ans) {
//         console.log("You were not correct")
//         response.correct = 0;
//     }
// } else if (response.key == choices[1]) {
//     console.log('k was chosen')
//     response.choice = line_lens_order[1];
//     if (response.choice == corr_ans) {
//         console.log("You were correct")
//         response.correct = 1;
//     } else if (response.choice != corr_ans) {
//         console.log("You were not correct")
//         response.correct = 0;
//     }
// } else if (response.key == choices[2]) {
//     console.log(response.key)
//     console.log('l was chosen')
//     response.choice = line_lens_order[2];
//     console.log(response.choice)
//     if (response.choice == corr_ans) {
//         console.log("You were correct")
//         response.correct = 1;
//     } else if (response.choice != corr_ans) {
//         console.log("You were not correct")
//         response.correct = 0;
//     }
// }
