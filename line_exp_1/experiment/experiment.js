// 1 category experiment - line_cat_multi_stim_
// testing the attraction effect
// Sean Conway
// this file contains the source code. Utilizes jspsych plugins (including two custom ones)
// click index.html (or the shell script that initializes it) to run

// CONSTANTS - Get date of participation, assign subject a random participation number
var TODAY = new Date();
var DD = String(TODAY.getDate()).padStart(2, '0');
var MM = String(TODAY.getMonth() + 1).padStart(2, '0');
var YYYY = TODAY.getFullYear();
const DATE = YYYY + MM + DD;

// only need this for online running
// var part_ID = jsPsych.randomization.randomID(5);
// Adding  important experiment information
// Subject number, which category name was which, etc
jsPsych.data.addProperties({
    // sub_n: part_ID,
    date: DATE,
});

// subject number prompt
var enter_sub_n = {
  data: {
      screen_id: "enter-sub-number"
  },
  type: 'survey-html-form',
  preamble: "<p>Please enter in a UNIQUE participant number.</p>",
  html: '<p>Participant Number: <input type="text" id="test-resp-box" name="response" size="10" /></p>',
  autofocus: 'test-resp-box',
  on_finish: function(data){
    jsPsych.data.addProperties({
      sub_n: data.response["response"]
    })
  }
}

// preloading some image files
var img_preload = {
    type: 'preload',
    images: ['img/line_ex1.png','img/line_ex2.png','img/line_ex3.png']
}

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
            " There's some uncertainty involved in this category. You can't be 100% certain whether or not an object is in the category."+
            " BUT, some objects are far more likely to belong in the category than others."+
            "<p>Your job is to learn this category and then use what you have learned to " +
            "decide if each line belongs in the category or not.</p><br>" +
            "<p>Click the continue button to continue the instructions.</p>",
            "<p><b>Learning the category</b></p><br>" +
            "<p>The experiment is broken into two phases.In the first phase, you will learn the category.</p><br>" +
            "<p>We will begin by showing examples of lines that BELONG to the category and lines that DO NOT BELONG to the category. Please study these closely.</p>"+
            "<p>After this, the learning trials will start.</p>" +
            "<p>Before each trial, a plus sign will appear on the screen. This will let" +
            " you know that a new trial is starting. When the trial begins, you will be shown an line. Press <b>'U'</b> on the keyboard if the line DOES belong to the category, and <b>'I'</b> if it DOES NOT belong to the category." +
            " At first you will be guessing. However, after each trial, you will be" +
            " told whether or not your answer was correct and whether or not the line belongs in the category. This feedback will allow you to learn about the" +
            " category.</p>"+
            "<p>Don't get discouraged! Like we said, there is uncertainty in this category. Your job is not to perform perfectly, but to learn which types of stimuli are more likely to be in the category than others</p>"+
            "<p>With that being said, please try do your best.</p>",
            "<p>There will be another section in which you will use what you've learned to make more categorizations - but instructions for that will come later.</p>"+
            "<p>The answers you provide today will contribute to the advancement of psychological science, and we thank you.  To obtain meaningful patterns in the data, active engagement on your part is necessary.  We are relying on your undivided attention!</p><br>",
            "<p>If you have any questions, now would be a good time to ask the experimenter.</p>",
            "<p>On the next screen, you will be shown examples of lines that belong to the category, and examples of lines that don't belong to the category.</p>"+
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
    in_category: in_category,
    out_category: out_category,
    duration: 60000
}

// learning trials
var learn = {
    data: {
        screen_id: 'learn_trial',
        line_length: jsPsych.timelineVariable('line_len'),
        cat_trial_type: 'learning',
        jnds: jsPsych.timelineVariable('jnds')
    },
    type: 'categorize-lines',
    line_lens: [jsPsych.timelineVariable('line_len')],
    stim_names: [''],
    choices: ['u', 'i'],
    in_cat: jsPsych.timelineVariable('distribution'),
    give_fb: true,
    fb_dur_corr: 1000,
    fb_dur_incorr: 3000,
    dist_btw: 200,
    prompt: "Does this line belong in the category? (U=YES, I=NO)"
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
            " you will be presented with multiple lines and you will determine the line that BEST belongs to the" +
            " category. The only other difference from the learning trials is that you will" +
            " not be told whether or not your answer was correct.</p><br>" +
            "<p>Please remember to try your best!</p>"+
            "<p>Thank you, and good luck!</p>"
        ]
    }, ],
};

var transfer = {
    data: {
        screen_id: 'transfer_trial',
        T_psy: jsPsych.timelineVariable('target_psy'),
        T_phys: jsPsych.timelineVariable('target_phys'),
        C_psy: jsPsych.timelineVariable('competitor_psy'),
        C_phys: jsPsych.timelineVariable('competitor_phys'),
        D_psy: jsPsych.timelineVariable('decoy_psy'),
        D_phys: jsPsych.timelineVariable('decoy_phys'),
        corr_choice: jsPsych.timelineVariable('correct'),
        correct_psy: jsPsych.timelineVariable('correct_psy'),
        cat_trial_type: 'transfer',
        stim_1: jsPsych.timelineVariable('stim_1'),
        stim_2: jsPsych.timelineVariable('stim_2'),
        stim_3: jsPsych.timelineVariable('stim_3'),
        stim_1_psy: jsPsych.timelineVariable('stim_1_psy'),
        stim_2_psy: jsPsych.timelineVariable('stim_2_psy'),
        stim_3_psy: jsPsych.timelineVariable('stim_3_psy'),
        transfer_trial_type: jsPsych.timelineVariable('trial_type'),
        trial_choice_set: jsPsych.timelineVariable('trial_choice_set'),
        difficulty_level: jsPsych.timelineVariable('difficulty_level'),
        target_side: jsPsych.timelineVariable('target_side'),
        competitor_side: jsPsych.timelineVariable('competitor_side'),
        tc_correct: jsPsych.timelineVariable('tc_correct'),
        correct_dist_to_mean: jsPsych.timelineVariable('correct_dist_to_mean'),
        side_correct: jsPsych.timelineVariable('side_correct')
    },
    type: 'categorize-lines',
    line_lens: function() {
        if (jsPsych.timelineVariable('stim_3') == 0) {
            var line_lens = [
              jsPsych.timelineVariable('stim_1'),
              jsPsych.timelineVariable('stim_2')
            ]
        } else if (jsPsych.timelineVariable('stim_3') > 0) {
            var line_lens = [
              jsPsych.timelineVariable('stim_1'),
              jsPsych.timelineVariable('stim_2'),
              jsPsych.timelineVariable('stim_3')
            ]
        }
        return line_lens
    },
    stim_names: function() {
        if (jsPsych.timelineVariable('stim_3') == 0) {
            var choices = ['J', 'K']
        } else if (jsPsych.timelineVariable('stim_3') > 0) {
            var choices = ['J', 'K', 'L']
        }
        return choices
    },
    choices: function() {
        if (jsPsych.timelineVariable('stim_3') == 0) {
            var choices = ['j', 'k'];
        } else if (jsPsych.timelineVariable('stim_3') > 0) {
            var choices = ['j', 'k', 'l']
        }
        return choices
    },
    give_fb: false,
    corr_ans: jsPsych.timelineVariable('correct'),
    dist_btw: 200,
    //prompt: "Which line is most likely to belong in the category?",
    prompt: function(data){
      console.log(jsPsych.timelineVariable("tc_correct"));
      return "T="+jsPsych.timelineVariable('target_phys')+", C="+jsPsych.timelineVariable("competitor_phys")+", D="+jsPsych.timelineVariable("decoy_phys");
    },
    on_finish: function(data) {
      if(jsPsych.timelineVariable('trial_type')=='attraction'){
        console.log("ATTRACTION")
        console.log("target is "+data.T_phys)
        console.log("competitor is "+data.C_phys)
        console.log("decoy is "+data.D_phys)
        console.log('choice was ' + data.choice)
        if (data.choice == data.T_phys) {
          data.attr_choice = 'target'
          console.log("TARGET CHOSEN")
        } else if (data.choice == data.C_phys) {
          data.attr_choice = 'competitor'
          console.log("COMPETITOR CHOSEN")
        } else if (data.choice == data.D_phys) {
          data.attr_choice = 'decoy'
          console.log("DECOY CHOSEN")
        } else{
          console.log("ERROR")
          data.attr_choice = 'ERROR-t/c/d could not be determined'
        }
      }else{
        console.log("FILLER")
        data.attr_choice = 'NA'
      }
    },
}

var instr_procedure = {
  timeline: [img_preload, instructions, demo, pre_exp_mssg],
  repetitions: 1
}

var learn_procedure = {
    timeline: [learn],
    timeline_variables: learn_trials,
    repetitions: 1,
    randomize_order: true
};

var transfer_procedure = {
    timeline: [transfer],
    timeline_variables: transfer_trials,
    repetitions: 1,
    randomize_order: true
};

// var elephant = {
//     data: {
//         screen_id: "elephant"
//     },
//     type: 'image-keyboard-response',
//     stimulus: 'elephant.png',
//     choices: jsPsych.NO_KEYS,
//     trial_duration: 10000,
//     prompt: "<p>You have completed the experiment.</p>" +
//         "<p>The elephant says: don't forget to write down your participant ID (which will be coming shortly).</p>"
// };

// // Participant ID screen
// var id_screen = {
//     data: {
//         screen_id: "participant_id_screen"
//     },
//     type: "instructions",
//     pages: [
//         "<p>You have completed the experiment.</p>" +
//         "<p>Thank you for your participation.</p>" +
//         "<p>Your Participation ID is " + part_ID + "</p>" +
//         "<p>Please write this down and use the Zoom chat to tell the research assistant this number.</p>" +
//         "<p>The last page of this experiment is the debriefing form.</p>" +
//         "<p>After you have finished reading it, you can press the <b>J</b> key to close the experiment.</p>"
//     ],
//     show_clickable_nav: true
// };

var exp_finished_prompt = {
  data: {
    screen_id: "exp_finished_prompt"
  },
  type: "instructions",
  pages: [
    "<p>You have completed the experiment! Thank you for your participation.</p>"+
    "<p>On the next page is the debriefing form. You are welcome to read it for as long as you wish.<b>Please press the 'j' key when you are done reading the debriefing.</b></p>",
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
    stimulus: 'img/Cons_choice_debrief.png',
    prompt: "",
    trial_duration: 200000,
    response_ends_trial: true
}


timeline = [];

// init full screen mode
timeline.push({
 data: {
      screen_id: "enter-fullscreen-mode"
  },
 type: 'fullscreen',
 fullscreen_mode: true
});
//
// // request subject number
//timeline.push(enter_sub_n);
// // // //
// // // // // begin learning instructions
// timeline.push(instr_procedure);
// // // //
// // // // // begin learning
// timeline.push(learn_procedure);
// // // //
// // // // // begin transfer instructions
// timeline.push(transfer_instructions);
// //
// begin transfer
timeline.push(transfer_procedure);

//for online experiments - make sure they take note of participant ID
// timeline.push(elephant);
// timeline.push(id_screen);

timeline.push(exp_finished_prompt);

// debrief subjects
timeline.push(debrief);

// init experiment
jsPsych.init({
    timeline: timeline,
    // download data
    on_finish: function (data){
      var csv = jsPsych.data.get().csv();
      var file_name = "line_cat_multi_stim_"+ jsPsych.data.get().last(1).values()[0].sub_n + ".csv";
      download_csv(csv, file_name);
      //jsPsych.data.displayData(); //comment this out before running
    }
});
