// 1 category experiment - line_cat_multi_stim_2
// testing the attraction effect
// follow up experiment
// Sean Conway
// this file contains the source code for experiment 2.
// Utilizes jspsych plugins (including two custom ones)
// click index.html (or the shell script that initializes it) to run

// CONSTANTS
var TODAY = new Date();
var DD = String(TODAY.getDate()).padStart(2, '0');
var MM = String(TODAY.getMonth() + 1).padStart(2, '0');
var YYYY = TODAY.getFullYear();
const DATE = YYYY + MM + DD;
const fb_dur_corr = 1000
const fb_dur_incorr = 3000

// Adding  important experiment information
jsPsych.data.addProperties({
    // sub_n: part_ID,
    exp_name: "line_cat_multi_stim_2",
    date: DATE,
});

// init full screen mode
var fullscreen_mode = {
    data: {
        screen_id: "enter-fullscreen-mode"
    },
    type: 'fullscreen',
    fullscreen_mode: true
}

// subject number prompt
var enter_sub_n = {
    data: {
        screen_id: "enter-sub-number"
    },
    type: 'survey-html-form',
    preamble: "<p>Please enter in a UNIQUE participant number.</p>",
    html: '<p>Participant Number: <input type="text" id="test-resp-box" name="response" size="10" required/></p>',
    autofocus: 'test-resp-box',
    on_finish: function(data) {
        jsPsych.data.addProperties({
            sub_n: data.response["response"]
        })
    }
}

// preloading some image files
var img_preload = {
    type: 'preload',
    images: ['img/line_ex1.png', 'img/line_ex2.png', 'img/line_ex3.png']
}

function check_consent(elem) {

    if (document.getElementById('consent_check_agree').checked) {
        return true
    } else if (document.getElementById('consent_check_not_agree').checked) {
        alert("If you do not wish to participate in the study, you are free to go. Please let the experimenter know.");
    } else {
        alert("Please indicate whether or not you wish to participate in this study.")
    }
}

var consent = {
    type: 'external-html',
    url: "consent.html",
    cont_btn: "start",
    check_fn: check_consent
};

// instructions - mostly just goes over learning
var instructions = {
    data: {
        screen_id: "instructions"
    },
    type: "instructions",
    choices: ['Continue'],
    show_clickable_nav: true,
    button_label_next: "Continue",
    button_label_previous: "Previous Page",
    pages: ["<p>Welcome! Thank you for participating!</p>",
        "<p><b>The Stimuli:</b></p><br>" +
        "<p>In this experiment, you will be asked to learn which objects belong to a category and which objects do not belong to the category.</p>" +
        "<p>The objects you will be asked to categorize are lines of varying length. Something like this:</p>" +
        "<div style='float: center;'><img src='img/line_ex1.png'width=75 height=200/> " +
        "<div style='float: center;'><img src='img/line_ex2.png'width=75 height=200/> " +
        "<div style='float: center;'><img src='img/line_ex3.png'width=75 height=200/>",
        "<p>Each line <b>may or may not</b> belong in the category." +
        " The length of each line determines whether or not it belongs to the category.</p><br>" +
        " There's some uncertainty involved in this category. You can't be 100% certain whether or not an object is in the category." +
        " BUT, some objects are far more likely to belong in the category than others." +
        "<p>Your job is to learn this category and then use what you have learned to " +
        "decide if each line belongs in the category or not.</p><br>" +
        "<p>Click the continue button to continue the instructions.</p>",
        "<p><b>Learning the category</b></p><br>" +
        "<p>The experiment is broken into two phases.In the first phase, you will learn the category.</p><br>" +
        "<p>We will begin by showing examples of lines that BELONG to the category and lines that DO NOT BELONG to the category. Please study these closely.</p>" +
        "<p>After this, the learning trials will start.</p>" +
        "<p>Before each trial, a plus sign. This will let" +
        " you know that a new trial is starting. When the trial begins, you will be shown an line. Press <b>'U'</b> on the keyboard if the line DOES belong to the category, and <b>'I'</b> if it DOES NOT belong to the category." +
        " At first you will be guessing. However, after each trial, you will be" +
        " told whether or not your answer was correct and whether or not the line belongs in the category. This feedback will allow you to learn about the" +
        " category.</p>" +
        "<p>Don't get discouraged! Like we said, there is uncertainty in this category. Your job is not to perform perfectly, but to learn which types of stimuli are more likely to be in the category than others</p>" +
        "<p>With that being said, please try do your best.</p>",
        "<p>There will be another section in which you will use what you've learned to make more categorizations - but instructions for that will come later.</p>" +
        "<p>The answers you provide today will contribute to the advancement of psychological science, and we thank you.  To obtain meaningful patterns in the data, active engagement on your part is necessary.  We are relying on your undivided attention!</p><br>",
        "<p>If you have any questions, now would be a good time to ask the experimenter.</p>",
        "<p>On the next screen, you will be shown examples of lines that belong to the category, and examples of lines that don't belong to the category.</p>" +
        "<p>These lines will be on the screen for a full minute, and then the experiment will start. <b>Please study these lines closely.</b>"
    ]
};

// pre - experiment message
var pre_exp_mssg = {
    data: {
        screen_id: "click-to-begin-the-experiment"
    },
    type: 'instructions',
    pages: [
        'Click to begin the experiment.'
    ],
    show_clickable_nav: true
}

// drawing "demo" lines to jumpstart learning
var demo = {
    data: {
        screen_id: "demo-lines"
    },
    type: 'draw-demo-lines',
    in_category: in_cat_trans,
    out_category: out_cat_trans,
    duration: 60000
}

var instr_procedure = {
    timeline: [img_preload, instructions, demo, pre_exp_mssg],
    repetitions: 1
}


// learning trials
var learn = {
    data: {
        screen_id: 'learn_trial',
        line_length: jsPsych.timelineVariable('line_len'),
        exp_trial_type: 'learning',
        jnds: jsPsych.timelineVariable('jnds'),
        dens: jsPsych.timelineVariable('dens'),
        distribution: jsPsych.timelineVariable('distribution')
    },
    type: 'categorize-lines-2',
    stim_vals: [jsPsych.timelineVariable('line_len')],
    scale_stim: false,
    stim_names: [''],
    choices: ['u', 'i'],
    in_cat: jsPsych.timelineVariable('distribution'),
    give_fb: true,
    fb_dur_corr: fb_dur_corr,
    fb_dur_incorr: fb_dur_incorr,
    dist_btw: 200,
    prompt: "Does this line belong in the category? (U=YES, I=NO)",
    on_finish: function(data){
      var csv = jsPsych.data.get().csv();
      var file_name = "line_cat_multi_stim_2_" + jsPsych.data.get().last(1).values()[0].sub_n + ".csv";
      download_csv(csv, file_name);
    }
};

var learn_procedure = {
    timeline: [learn],
    timeline_variables: learn_trials,
    repetitions: 1,
    randomize_order: true
};


// transfer instructions
var transfer_instructions = {
    timeline: [{
        data: {
            screen_id: "transfer_instructions"
        },
        type: "instructions",
        choices: ['Continue'],
        show_clickable_nav: true,
        button_label_next: "Continue",
        button_label_previous: "Previous Page",
        pages: ["<p>The learning phase is now complete.</p>",
            "<p><b>Using what you've learned</b></p><br>" +
            "<p>Now that you have learned the category, we will ask you to use what" +
            " you've learned to categorize some new lines. During these trials" +
            " you will be presented with multiple lines and you will determine the line that BEST belongs to the category.</p>" +
            "<p>The category is <b>the same as you learned in training.</b>. The only difference is now you are choosing between multiple options, rather than just one.</p>" +
            "<p>You will still receive feedback after each choice.</p>" +
            "<p>Please remember to try your best!</p>" +
            "<p>Thank you, and good luck!</p>"
        ]
    }, ],
};

// transfer trials
var transfer = {
    data: {
        screen_id: 'transfer_trial',
        correct_1_psy: jsPsych.timelineVariable('correct_1_psy'),
        correct_2_psy: jsPsych.timelineVariable('correct_2_psy'),
        correct_1_phy: jsPsych.timelineVariable('correct_1_phy'),
        correct_2_psy: jsPsych.timelineVariable('correct_2_phy'),
        exp_trial_type: 'transfer',
        transfer_trial_type: jsPsych.timelineVariable('transfer_trial_type'),
        trial_choice_set: jsPsych.timelineVariable('trial_choice_set'),
        difficulty: jsPsych.timelineVariable('difficulty'),
        target_side: jsPsych.timelineVariable('target_side'),
        competitor_side: jsPsych.timelineVariable('competitor_side'),
        tc_correct: jsPsych.timelineVariable('tc_correct'),
        side_correct: jsPsych.timelineVariable('side_correct'),
        stim_1_psy: jsPsych.timelineVariable('stim_1_psy'),
        stim_2_psy: jsPsych.timelineVariable('stim_2_psy'),
        stim_3_psy: jsPsych.timelineVariable('stim_3_psy'),
        stim_1_phy: jsPsych.timelineVariable('stim_1_phy'),
        stim_2_phy: jsPsych.timelineVariable('stim_2_phy'),
        stim_3_phy: jsPsych.timelineVariable('stim_3_phy'),
        target_phy: jsPsych.timelineVariable('target_phy'),
        competitor_phy: jsPsych.timelineVariable('competitor_phy'),
        decoy_phy: jsPsych.timelineVariable('decoy_phy'),
        target_psy: psy2phy(jsPsych.timelineVariable('target_phy')),
        competitor_psy: psy2phy(jsPsych.timelineVariable('competitor_phy')),
        decoy_psy: psy2phy(jsPsych.timelineVariable('decoy_phy'))
    },
    type: 'categorize-lines-2',
    scale_stim: false,
    stim_vals: function(data) {
        if (jsPsych.timelineVariable('transfer_trial_type') == 'filler_sample') {
            if (jsPsych.timelineVariable('trial_choice_set') == 'binary') {
                var stim_vals = [psy2phy(sample_in()), psy2phy(sample_out())];
            } else if (jsPsych.timelineVariable('trial_choice_set') == 'trinary') {
                var stim_vals = [psy2phy(sample_in()), psy2phy(sample_out()), psy2phy(sample_out())]; // codes I'm making up to tell the plugin to sample
            }
        } else {
            if (jsPsych.timelineVariable('trial_choice_set') == 'binary') {
                var stim_vals = [
                    jsPsych.timelineVariable('stim_1_phy'),
                    jsPsych.timelineVariable('stim_2_phy')
                ]
            } else if (jsPsych.timelineVariable('trial_choice_set') == 'trinary') {
                var stim_vals = [
                    jsPsych.timelineVariable('stim_1_phy'),
                    jsPsych.timelineVariable('stim_2_phy'),
                    jsPsych.timelineVariable('stim_3_phy')
                ]
            }
        }
        return stim_vals
    },
    stim_names: function() {
        if (jsPsych.timelineVariable('trial_choice_set') == 'binary') {
            var choices = ['J', 'K']
        } else if (jsPsych.timelineVariable('trial_choice_set') == 'trinary') {
            var choices = ['J', 'K', 'L']
        }
        return choices
    },
    choices: function() {
        if (jsPsych.timelineVariable('trial_choice_set') == 'binary') {
            var choices = ['j', 'k'];
        } else if (jsPsych.timelineVariable('trial_choice_set') == 'trinary') {
            var choices = ['j', 'k', 'l']
        }
        return choices
    },
    give_fb: true,
    corr_ans: function(data) {
        if (jsPsych.timelineVariable('transfer_trial_type') == "filler_sample") {
            var corr_ans = "sampled_1";
        } else {
            var corr_ans = [jsPsych.timelineVariable('correct_1_phy'),
                jsPsych.timelineVariable('correct_2_phy')
            ]
        }
        return corr_ans
    },
    dist_btw: 200,
    prompt: "Which line is most likely to belong in the category?",
    fb_dur_corr: 500,
    fb_dur_incorr: 1500,
    target_phys: jsPsych.timelineVariable('target_phy'),
    competitor_phys: jsPsych.timelineVariable('competitor_phy'),
    decoy_phys: jsPsych.timelineVariable('decoy_phy'),
    transfer_trial_type: jsPsych.timelineVariable('transfer_trial_type'),
    on_start: function(){
      if(jsPsych.timelineVariable('target_side')=="left"){
        console.log("target is short, competitor is long.")
      }else if(jsPsych.timelineVariable('target_side')=="right"){
        console.log("target is long, competitor is short.")
      }
      if(jsPsych.timelineVariable('tc_correct')=="target"){
        console.log("target correct")
      }else if(jsPsych.timelineVariable('tc_correct')=="competitor"){
        console.log("competitor correct")
      }if(jsPsych.timelineVariable('tc_correct')=="both"){
        console.log("both correct")
      }
    },
    on_finish: function(data) {
        if (jsPsych.timelineVariable('transfer_trial_type') == 'attraction') {
            if (data.choice == data.target_phy) {
                data.attr_choice = 'target'
                //console.log(data.attr_choice)
            } else if (data.choice == data.competitor_phy) {
                data.attr_choice = 'competitor'
                //console.log(data.attr_choice)
            } else if (data.choice == data.decoy_phy) {
                data.attr_choice = 'decoy'
                //console.log(data.attr_choice)
            } else {
                console.log("ERROR")
                data.attr_choice = 'ERROR-t/c/d could not be determined'
            }
            console.log("attraction choice=",data.attr_choice);
        } else {
            console.log("FILLER")
            data.attr_choice = 'NA'
        }
        // var csv = jsPsych.data.get().csv();
        // var file_name = "line_cat_multi_stim_2_" + jsPsych.data.get().last(1).values()[0].sub_n + ".csv";
        // download_csv(csv, file_name);
    },
}

var transfer_procedure = {
    timeline: [transfer],
    timeline_variables: transfer_trials,
    repetitions: 3,
    randomize_order: true
};

var survey_instructions = {
    type: "instructions",
    pages: [
        "<p>Thank you so much for participating in the experiment!</p>",
        "<p>Before you finish, we just have a few questions we would like you to answer.</p>" +
        "<p>There are no right or wrong answers. You will get credit for the study regardless of your answers. We are just interested in what you thought of the task and how you went about doing it.</p>" +
        "<p>For free response questions, feel free to write as little or as much as you would like.</p>" +
        "<p>Thank you again for participating!</p>"
    ],
    button_label_next: "Continue",
    show_clickable_nav: true
}

var attr_eff_example = {
    data: {
        screen_id: "attr_eff_example"
    },
    type: 'survey-html-form',
    preamble: "<p>Here is an example of a trial you may have seen during the experiment.</p><div style='float: center;'><img src='img/example_attr_eff_cropped.png' height=500 /> <p>Which option do you think you would have picked? J,K, or L?</p>",
    html: "<p><input type=radio name=example_attr_eff_choice value=target required>J</p>" +
        "<p><input type=radio name=example_attr_eff_choice value=decoy>K</p>" +
        "<p><input type=radio name=example_attr_eff_choice value=competitor>L</p></p>" +
        "<p>Why did you choose that option?</p> <p><input type='text' id='attr_eff_example_justify' name='response' size=75 required/></p>"
}

var learn_free_response = {
    type: 'survey-text',
    questions: [{
        prompt: "<p>In the first part of the experiment, you saw one line at a time and decided whether or not it was in the category.</p>" +
            "<p>How did you go about doing the task during this first set of trials?</p>",
        name: "learn_free_response",
        required: true
    }]
}

var learn_strategy = {
    type: 'survey-multi-choice',
    questions: [{
        prompt: "<p>Which of the following best describes how you went about doing the task during the first set of trials? (i.e., one line per trial)</p>",
        name: "learn_strategy",
        options: ['I was completely guessing.',
            'I used a simple mental rule to determine if I should respond yes or no to a given line.',
            'I used my intuition/my experience receiving feedback to figure out if the line was likely to be in the category.',
            'I tried to memorize which kinds of lines were in the category and which kinds of lines were not in the category.',
            'None of these describe how I did the task.'
        ],
        required: true
    }]
}

var transfer_free_response = {
    type: 'survey-text',
    questions: [{
        prompt: "<p>In the second part of the experiment, you saw 2-3 lines at a time and chose the line that was most likely to be in the category.</p>" +
            "<p>How did you go about doing the task during this second part of the experiment? (i.e., two/three lines per trial)</p>",
        name: "transfer_free_response",
        required: true
    }]
}


var transfer_strategy = {
    type: 'survey-multi-choice',
    questions: [{
        prompt: "<p>Which of the following best describes how you went about doing the task during this SECOND set of trials?</p>",
        name: "transfer_strategy",
        options: [
            "I tried to pick the shortest line.",
            "I tried to pick the longest line.",
            "I tried to pick a line that was neither very long nor very short.",
            'I tried to memorize which kinds of lines were in the category and which types of lines were not in the category.',
            'I used my intuition/my experience receiving feedback to figure out which line was most likely to be in the category.',
            "I tried to pick a line that was either very short or very long.",
            "I was guessing.",
            "None of these describe how I did the task."
        ],
        required: true
    }]
}


var survey_procedure = {
    timeline: [survey_instructions, attr_eff_example, learn_free_response, learn_strategy, transfer_free_response, transfer_strategy],
    repetitions: 1
}

var exp_finished_prompt = {
    data: {
        screen_id: "exp_finished_prompt"
    },
    type: "instructions",
    pages: [
        "<p>You have completed the experiment! Thank you for your participation.</p>" +
        "<p>On the next page is the debriefing form. You are welcome to read it for as long as you wish." +
        "<b>Please press the 'j' key when you are done reading the debriefing.</b></p>",
        "<p>DO NOT CLOSE OUT OF THE EXPERIMENT. The experimenter will handle that for you. Instead, simply press the <b>'j'</b> key when you are finished reading the debriefing form.</p>"
    ],
    show_clickable_nav: true,
}

var debrief = {
    data: {
        screen_id: "debrief"
    },
    type: "image-keyboard-response",
    choices: ['j'],
    stimulus: 'img/Judg_choice_debrief.png',
    prompt: "",
    trial_duration: 200000,
    response_ends_trial: true
}


timeline = [];

// make fullscreen
//timeline.push(fullscreen_mode);

// request subject number
// timeline.push(enter_sub_n);
// //
// // consent
// timeline.push(consent)
// //
// // // begin learning instructions
timeline.push(instr_procedure);
// // //
// // //  begin learning
// timeline.push(learn_procedure);
// //
// // // // //  begin transfer instructions
// // timeline.push(transfer_instructions);
// // //
// // // // begin transfer
timeline.push(transfer_procedure);
// //
// // // post experiment survey
timeline.push(survey_procedure);
// //
// // // let people know the experiment is finished
timeline.push(exp_finished_prompt);

// debrief subjects
timeline.push(debrief);

// init experiment
jsPsych.init({
    timeline: timeline,
    // download data
    on_finish: function(data) {
        var csv = jsPsych.data.get().csv();
        var file_name = "line_cat_multi_stim_2_" + jsPsych.data.get().last(1).values()[0].sub_n + ".csv";
        download_csv(csv, file_name);
        //jsPsych.data.displayData(); //comment this out before running
    }
});
