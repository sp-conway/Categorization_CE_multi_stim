#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
This experiment was created using PsychoPy3 Experiment Builder (v2024.2.2),
    on October 10, 2024, at 15:29
If you publish work using this script the most relevant publication is:

    Peirce J, Gray JR, Simpson S, MacAskill M, Höchenberger R, Sogo H, Kastman E, Lindeløv JK. (2019) 
        PsychoPy2: Experiments in behavior made easy Behav Res 51: 195. 
        https://doi.org/10.3758/s13428-018-01193-y

"""

# --- Import packages ---
from psychopy import locale_setup
from psychopy import prefs
from psychopy import plugins
plugins.activatePlugins()
prefs.hardware['audioLib'] = 'ptb'
prefs.hardware['audioLatencyMode'] = '3'
from psychopy import sound, gui, visual, core, data, event, logging, clock, colors, layout, hardware
from psychopy.tools import environmenttools
from psychopy.constants import (NOT_STARTED, STARTED, PLAYING, PAUSED,
                                STOPPED, FINISHED, PRESSED, RELEASED, FOREVER, priority)

import numpy as np  # whole numpy lib is available, prepend 'np.'
from numpy import (sin, cos, tan, log, log10, pi, average,
                   sqrt, std, deg2rad, rad2deg, linspace, asarray)
from numpy.random import random, randint, normal, shuffle, choice as randchoice
import os  # handy system and path functions
import sys  # to get file system encoding

from psychopy.hardware import keyboard

# Run 'Before Experiment' code from randomize_condition
import numpy as np
exp_conditions_all=np.array(['centered','left','right'])
exp_condition=np.random.choice(exp_conditions_all)

if(exp_condition=='centered'):
    demo_in_condition_file='conditions/learn_demo_in_centered.txt'
    demo_out_condition_file='conditions/learn_demo_out_centered.txt'
    learn_condition_file='conditions/learn_trials_centered.csv'
    transfer_condition_file='conditions/transfer_trials_centered.csv'
elif(exp_condition=='left'):
    demo_in_condition_file='conditions/learn_demo_in_left.txt'
    demo_out_condition_file='conditions/learn_demo_out_left.txt'
    learn_condition_file='conditions/learn_trials_left.csv'
    transfer_condition_file='conditions/transfer_trials_left.csv'
elif(exp_condition=='right'):
    demo_in_condition_file='conditions/learn_demo_in_right.txt'
    demo_out_condition_file='conditions/learn_demo_out_right.txt'
    learn_condition_file='conditions/learn_trials_right.csv'
    transfer_condition_file='conditions/transfer_trials_right.csv'

# Run 'Before Experiment' code from define_trial_vars
dist_btw=200
line_width=2
demo_dur=30
dist_btw_demo=25


# --- Setup global variables (available in all functions) ---
# create a device manager to handle hardware (keyboards, mice, mirophones, speakers, etc.)
deviceManager = hardware.DeviceManager()
# ensure that relative paths start from the same directory as this script
_thisDir = os.path.dirname(os.path.abspath(__file__))
# store info about the experiment session
psychopyVersion = '2024.2.2'
expName = 'line_categorization_exp_updated'  # from the Builder filename that created this script
# information about this experiment
expInfo = {
    'participant': '',
    'computer_number': '',
    'date|hid': data.getDateStr(),
    'expName|hid': expName,
    'psychopyVersion|hid': psychopyVersion,
}

# --- Define some variables which will change depending on pilot mode ---
'''
To run in pilot mode, either use the run/pilot toggle in Builder, Coder and Runner, 
or run the experiment with `--pilot` as an argument. To change what pilot 
#mode does, check out the 'Pilot mode' tab in preferences.
'''
# work out from system args whether we are running in pilot mode
PILOTING = core.setPilotModeFromArgs()
# start off with values from experiment settings
_fullScr = True
_winSize = [1920, 1080]
# if in pilot mode, apply overrides according to preferences
if PILOTING:
    # force windowed mode
    if prefs.piloting['forceWindowed']:
        _fullScr = False
        # set window size
        _winSize = prefs.piloting['forcedWindowSize']

def showExpInfoDlg(expInfo):
    """
    Show participant info dialog.
    Parameters
    ==========
    expInfo : dict
        Information about this experiment.
    
    Returns
    ==========
    dict
        Information about this experiment.
    """
    # show participant info dialog
    dlg = gui.DlgFromDict(
        dictionary=expInfo, sortKeys=False, title=expName, alwaysOnTop=True
    )
    if dlg.OK == False:
        core.quit()  # user pressed cancel
    # return expInfo
    return expInfo


def setupData(expInfo, dataDir=None):
    """
    Make an ExperimentHandler to handle trials and saving.
    
    Parameters
    ==========
    expInfo : dict
        Information about this experiment, created by the `setupExpInfo` function.
    dataDir : Path, str or None
        Folder to save the data to, leave as None to create a folder in the current directory.    
    Returns
    ==========
    psychopy.data.ExperimentHandler
        Handler object for this experiment, contains the data to save and information about 
        where to save it to.
    """
    # remove dialog-specific syntax from expInfo
    for key, val in expInfo.copy().items():
        newKey, _ = data.utils.parsePipeSyntax(key)
        expInfo[newKey] = expInfo.pop(key)
    
    # data file name stem = absolute path + name; later add .psyexp, .csv, .log, etc
    if dataDir is None:
        dataDir = _thisDir
    filename = u'data/%s_%s_%s' % (expInfo['participant'], expName, expInfo['date'])
    # make sure filename is relative to dataDir
    if os.path.isabs(filename):
        dataDir = os.path.commonprefix([dataDir, filename])
        filename = os.path.relpath(filename, dataDir)
    
    # an ExperimentHandler isn't essential but helps with data saving
    thisExp = data.ExperimentHandler(
        name=expName, version='',
        extraInfo=expInfo, runtimeInfo=None,
        originPath='C:\\Users\\Experiment\\Documents\\Experiments\\line_categorization_experiment\\line_categorization_exp_updated_lastrun.py',
        savePickle=True, saveWideText=True,
        dataFileName=dataDir + os.sep + filename, sortColumns='time'
    )
    thisExp.setPriority('thisRow.t', priority.CRITICAL)
    thisExp.setPriority('expName', priority.LOW)
    # return experiment handler
    return thisExp


def setupLogging(filename):
    """
    Setup a log file and tell it what level to log at.
    
    Parameters
    ==========
    filename : str or pathlib.Path
        Filename to save log file and data files as, doesn't need an extension.
    
    Returns
    ==========
    psychopy.logging.LogFile
        Text stream to receive inputs from the logging system.
    """
    # set how much information should be printed to the console / app
    if PILOTING:
        logging.console.setLevel(
            prefs.piloting['pilotConsoleLoggingLevel']
        )
    else:
        logging.console.setLevel('warning')
    # save a log file for detail verbose info
    logFile = logging.LogFile(filename+'.log')
    if PILOTING:
        logFile.setLevel(
            prefs.piloting['pilotLoggingLevel']
        )
    else:
        logFile.setLevel(
            logging.getLevel('info')
        )
    
    return logFile


def setupWindow(expInfo=None, win=None):
    """
    Setup the Window
    
    Parameters
    ==========
    expInfo : dict
        Information about this experiment, created by the `setupExpInfo` function.
    win : psychopy.visual.Window
        Window to setup - leave as None to create a new window.
    
    Returns
    ==========
    psychopy.visual.Window
        Window in which to run this experiment.
    """
    if PILOTING:
        logging.debug('Fullscreen settings ignored as running in pilot mode.')
    
    if win is None:
        # if not given a window to setup, make one
        win = visual.Window(
            size=_winSize, fullscr=_fullScr, screen=0,
            winType='pyglet', allowStencil=False,
            monitor='testMonitor', color='white', colorSpace='rgb',
            backgroundImage='', backgroundFit='none',
            blendMode='avg', useFBO=True,
            units='pix', 
            checkTiming=False  # we're going to do this ourselves in a moment
        )
    else:
        # if we have a window, just set the attributes which are safe to set
        win.color = 'white'
        win.colorSpace = 'rgb'
        win.backgroundImage = ''
        win.backgroundFit = 'none'
        win.units = 'pix'
    if expInfo is not None:
        # get/measure frame rate if not already in expInfo
        if win._monitorFrameRate is None:
            win._monitorFrameRate = win.getActualFrameRate(infoMsg='Attempting to measure frame rate of screen, please wait...')
        expInfo['frameRate'] = win._monitorFrameRate
    win.mouseVisible = True
    win.hideMessage()
    # show a visual indicator if we're in piloting mode
    if PILOTING and prefs.piloting['showPilotingIndicator']:
        win.showPilotingIndicator()
    
    return win


def setupDevices(expInfo, thisExp, win):
    """
    Setup whatever devices are available (mouse, keyboard, speaker, eyetracker, etc.) and add them to 
    the device manager (deviceManager)
    
    Parameters
    ==========
    expInfo : dict
        Information about this experiment, created by the `setupExpInfo` function.
    thisExp : psychopy.data.ExperimentHandler
        Handler object for this experiment, contains the data to save and information about 
        where to save it to.
    win : psychopy.visual.Window
        Window in which to run this experiment.
    Returns
    ==========
    bool
        True if completed successfully.
    """
    # --- Setup input devices ---
    ioConfig = {}
    ioSession = ioServer = eyetracker = None
    
    # store ioServer object in the device manager
    deviceManager.ioServer = ioServer
    
    # create a default keyboard (e.g. to check for escape)
    if deviceManager.getDevice('defaultKeyboard') is None:
        deviceManager.addDevice(
            deviceClass='keyboard', deviceName='defaultKeyboard', backend='ptb'
        )
    if deviceManager.getDevice('instr_cont1') is None:
        # initialise instr_cont1
        instr_cont1 = deviceManager.addDevice(
            deviceClass='keyboard',
            deviceName='instr_cont1',
        )
    if deviceManager.getDevice('instr_cont2') is None:
        # initialise instr_cont2
        instr_cont2 = deviceManager.addDevice(
            deviceClass='keyboard',
            deviceName='instr_cont2',
        )
    if deviceManager.getDevice('instr_cont3') is None:
        # initialise instr_cont3
        instr_cont3 = deviceManager.addDevice(
            deviceClass='keyboard',
            deviceName='instr_cont3',
        )
    if deviceManager.getDevice('instr_cont4') is None:
        # initialise instr_cont4
        instr_cont4 = deviceManager.addDevice(
            deviceClass='keyboard',
            deviceName='instr_cont4',
        )
    if deviceManager.getDevice('instr_cont5') is None:
        # initialise instr_cont5
        instr_cont5 = deviceManager.addDevice(
            deviceClass='keyboard',
            deviceName='instr_cont5',
        )
    if deviceManager.getDevice('instr_cont6') is None:
        # initialise instr_cont6
        instr_cont6 = deviceManager.addDevice(
            deviceClass='keyboard',
            deviceName='instr_cont6',
        )
    if deviceManager.getDevice('learn_start_reps') is None:
        # initialise learn_start_reps
        learn_start_reps = deviceManager.addDevice(
            deviceClass='keyboard',
            deviceName='learn_start_reps',
        )
    if deviceManager.getDevice('key_resp') is None:
        # initialise key_resp
        key_resp = deviceManager.addDevice(
            deviceClass='keyboard',
            deviceName='key_resp',
        )
    if deviceManager.getDevice('transfer_cont1') is None:
        # initialise transfer_cont1
        transfer_cont1 = deviceManager.addDevice(
            deviceClass='keyboard',
            deviceName='transfer_cont1',
        )
    if deviceManager.getDevice('transfer_cont2') is None:
        # initialise transfer_cont2
        transfer_cont2 = deviceManager.addDevice(
            deviceClass='keyboard',
            deviceName='transfer_cont2',
        )
    if deviceManager.getDevice('transfer_cont3') is None:
        # initialise transfer_cont3
        transfer_cont3 = deviceManager.addDevice(
            deviceClass='keyboard',
            deviceName='transfer_cont3',
        )
    if deviceManager.getDevice('key_resp_transfer_trinary') is None:
        # initialise key_resp_transfer_trinary
        key_resp_transfer_trinary = deviceManager.addDevice(
            deviceClass='keyboard',
            deviceName='key_resp_transfer_trinary',
        )
    if deviceManager.getDevice('key_resp_transfer_binary') is None:
        # initialise key_resp_transfer_binary
        key_resp_transfer_binary = deviceManager.addDevice(
            deviceClass='keyboard',
            deviceName='key_resp_transfer_binary',
        )
    if deviceManager.getDevice('cont_to_debrief') is None:
        # initialise cont_to_debrief
        cont_to_debrief = deviceManager.addDevice(
            deviceClass='keyboard',
            deviceName='cont_to_debrief',
        )
    if deviceManager.getDevice('end_debrief') is None:
        # initialise end_debrief
        end_debrief = deviceManager.addDevice(
            deviceClass='keyboard',
            deviceName='end_debrief',
        )
    # return True if completed successfully
    return True

def pauseExperiment(thisExp, win=None, timers=[], playbackComponents=[]):
    """
    Pause this experiment, preventing the flow from advancing to the next routine until resumed.
    
    Parameters
    ==========
    thisExp : psychopy.data.ExperimentHandler
        Handler object for this experiment, contains the data to save and information about 
        where to save it to.
    win : psychopy.visual.Window
        Window for this experiment.
    timers : list, tuple
        List of timers to reset once pausing is finished.
    playbackComponents : list, tuple
        List of any components with a `pause` method which need to be paused.
    """
    # if we are not paused, do nothing
    if thisExp.status != PAUSED:
        return
    
    # start a timer to figure out how long we're paused for
    pauseTimer = core.Clock()
    # pause any playback components
    for comp in playbackComponents:
        comp.pause()
    # make sure we have a keyboard
    defaultKeyboard = deviceManager.getDevice('defaultKeyboard')
    if defaultKeyboard is None:
        defaultKeyboard = deviceManager.addKeyboard(
            deviceClass='keyboard',
            deviceName='defaultKeyboard',
            backend='PsychToolbox',
        )
    # run a while loop while we wait to unpause
    while thisExp.status == PAUSED:
        # check for quit (typically the Esc key)
        if defaultKeyboard.getKeys(keyList=['escape']):
            endExperiment(thisExp, win=win)
        # sleep 1ms so other threads can execute
        clock.time.sleep(0.001)
    # if stop was requested while paused, quit
    if thisExp.status == FINISHED:
        endExperiment(thisExp, win=win)
    # resume any playback components
    for comp in playbackComponents:
        comp.play()
    # reset any timers
    for timer in timers:
        timer.addTime(-pauseTimer.getTime())


def run(expInfo, thisExp, win, globalClock=None, thisSession=None):
    """
    Run the experiment flow.
    
    Parameters
    ==========
    expInfo : dict
        Information about this experiment, created by the `setupExpInfo` function.
    thisExp : psychopy.data.ExperimentHandler
        Handler object for this experiment, contains the data to save and information about 
        where to save it to.
    psychopy.visual.Window
        Window in which to run this experiment.
    globalClock : psychopy.core.clock.Clock or None
        Clock to get global time from - supply None to make a new one.
    thisSession : psychopy.session.Session or None
        Handle of the Session object this experiment is being run from, if any.
    """
    # mark experiment as started
    thisExp.status = STARTED
    # make sure variables created by exec are available globally
    exec = environmenttools.setExecEnvironment(globals())
    # get device handles from dict of input devices
    ioServer = deviceManager.ioServer
    # get/create a default keyboard (e.g. to check for escape)
    defaultKeyboard = deviceManager.getDevice('defaultKeyboard')
    if defaultKeyboard is None:
        deviceManager.addDevice(
            deviceClass='keyboard', deviceName='defaultKeyboard', backend='PsychToolbox'
        )
    eyetracker = deviceManager.getDevice('eyetracker')
    # make sure we're running in the directory for this experiment
    os.chdir(_thisDir)
    # get filename from ExperimentHandler for convenience
    filename = thisExp.dataFileName
    frameTolerance = 0.001  # how close to onset before 'same' frame
    endExpNow = False  # flag for 'escape' or other condition => quit the exp
    # get frame duration from frame rate in expInfo
    if 'frameRate' in expInfo and expInfo['frameRate'] is not None:
        frameDur = 1.0 / round(expInfo['frameRate'])
    else:
        frameDur = 1.0 / 60.0  # could not measure, so guess
    
    # Start Code - component code to be run after the window creation
    
    # --- Initialize components for Routine "consent" ---
    agree_button = visual.ButtonStim(win, 
        text='I agree', font='Arvo',
        pos=(-75, -500),
        letterHeight=20.0,
        size=[100,100], 
        ori=0.0
        ,borderWidth=0.0,
        fillColor='darkgrey', borderColor=None,
        color='black', colorSpace='rgb',
        opacity=None,
        bold=True, italic=False,
        padding=None,
        anchor='center',
        name='agree_button',
        depth=0
    )
    agree_button.buttonClock = core.Clock()
    decline_button = visual.ButtonStim(win, 
        text='I do not agree', font='Arvo',
        pos=[75,-500],
        letterHeight=20.0,
        size=[100,100], 
        ori=0.0
        ,borderWidth=0.0,
        fillColor='darkgrey', borderColor=None,
        color='black', colorSpace='rgb',
        opacity=None,
        bold=True, italic=False,
        padding=None,
        anchor='center',
        name='decline_button',
        depth=-1
    )
    decline_button.buttonClock = core.Clock()
    consent_img = visual.ImageStim(
        win=win,
        name='consent_img', 
        image='consent.png', mask=None, anchor='bottom-center',
        ori=0.0, pos=(0, -475), draggable=False, size=[825,1000],
        color=[1,1,1], colorSpace='rgb', opacity=None,
        flipHoriz=False, flipVert=False,
        texRes=128.0, interpolate=True, depth=-2.0)
    mouse = event.Mouse(win=win)
    x, y = [None, None]
    mouse.mouseClock = core.Clock()
    
    # --- Initialize components for Routine "exp_setup" ---
    # Run 'Begin Experiment' code from get_screen_size
    import psychopy
    screen_w=win.size[0]
    screen_h=win.size[1]
    screen_c_x=screen_w/2
    screen_c_y=screen_h/2
    # Run 'Begin Experiment' code from libraries
    import numpy as np
    import pandas as pd
    
    # --- Initialize components for Routine "instructions1" ---
    instr1_1 = visual.TextStim(win=win, name='instr1_1',
        text='The stimuli:\nIn this experiment, you will be asked to learn which objects belong to a category and which objects do not belong to the category.',
        font='Open Sans',
        pos=[0,400], draggable=False, height=20.0, wrapWidth=None, ori=0.0, 
        color='black', colorSpace='rgb', opacity=None, 
        languageStyle='LTR',
        depth=0.0);
    instr_1_2 = visual.TextStim(win=win, name='instr_1_2',
        text='The objects you will be asked to categorize are lines of varying length. Something like this:',
        font='Open Sans',
        pos=[0,200], draggable=False, height=20.0, wrapWidth=None, ori=0.0, 
        color='black', colorSpace='rgb', opacity=None, 
        languageStyle='LTR',
        depth=-1.0);
    instr1_line1 = visual.Line(
        win=win, name='instr1_line1',
        size=[90, line_width],
        ori=90.0, pos=[0, 75], draggable=False, anchor='center',
        lineWidth=line_width,
        colorSpace='rgb', lineColor='black', fillColor='black',
        opacity=1.0, depth=-2.0, interpolate=True)
    instr1_line2 = visual.Line(
        win=win, name='instr1_line2',
        size=[150, line_width],
        ori=90.0, pos=[50, 10], draggable=False, anchor='center',
        lineWidth=line_width,
        colorSpace='rgb', lineColor='black', fillColor='black',
        opacity=None, depth=-3.0, interpolate=True)
    instr_cont1 = keyboard.Keyboard(deviceName='instr_cont1')
    instr_1_3 = visual.TextStim(win=win, name='instr_1_3',
        text="Press 'space' to continue.",
        font='Open Sans',
        pos=[0,-100], draggable=False, height=20.0, wrapWidth=None, ori=0.0, 
        color='black', colorSpace='rgb', opacity=None, 
        languageStyle='LTR',
        depth=-5.0);
    
    # --- Initialize components for Routine "instructions2" ---
    instr2_1 = visual.TextStim(win=win, name='instr2_1',
        text='The length of each line determines whether or not it belongs to the category.',
        font='Open Sans',
        pos=[0,400], draggable=False, height=20.0, wrapWidth=None, ori=0.0, 
        color='black', colorSpace='rgb', opacity=None, 
        languageStyle='LTR',
        depth=0.0);
    instr2_2 = visual.TextStim(win=win, name='instr2_2',
        text="There's some uncertainty involved in this category. You can't be 100% certain whether or not a line is in the category. BUT, some lines are far more likely to belong in the category than others.",
        font='Open Sans',
        pos=[0,200], draggable=False, height=20.0, wrapWidth=None, ori=0.0, 
        color='black', colorSpace='rgb', opacity=None, 
        languageStyle='LTR',
        depth=-1.0);
    instr2_3 = visual.TextStim(win=win, name='instr2_3',
        text='Your job is to learn this category and then use what you have learned to decide if each line belongs in the category or not.',
        font='Open Sans',
        pos=[0,0], draggable=False, height=20.0, wrapWidth=None, ori=0.0, 
        color='black', colorSpace='rgb', opacity=None, 
        languageStyle='LTR',
        depth=-2.0);
    instr2_4 = visual.TextStim(win=win, name='instr2_4',
        text="Press 'space' to continue the instructions.",
        font='Open Sans',
        pos=[0,-150], draggable=False, height=20.0, wrapWidth=None, ori=0.0, 
        color='black', colorSpace='rgb', opacity=None, 
        languageStyle='LTR',
        depth=-3.0);
    instr_cont2 = keyboard.Keyboard(deviceName='instr_cont2')
    
    # --- Initialize components for Routine "instructions3" ---
    instr3_1 = visual.TextStim(win=win, name='instr3_1',
        text='Learning the category',
        font='Open Sans',
        pos=[0,400], draggable=False, height=30.0, wrapWidth=None, ori=0.0, 
        color='black', colorSpace='rgb', opacity=None, 
        languageStyle='LTR',
        depth=0.0);
    instr3_2 = visual.TextStim(win=win, name='instr3_2',
        text='We will begin by showing examples of lines that BELONG to the category and lines that DO NOT BELONG to the category. Please study these closely.',
        font='Open Sans',
        pos=[0,200], draggable=False, height=20.0, wrapWidth=None, ori=0.0, 
        color='black', colorSpace='rgb', opacity=None, 
        languageStyle='LTR',
        depth=-1.0);
    instr3_3 = visual.TextStim(win=win, name='instr3_3',
        text="After this, the learning trials will start. When each trial begins, you will be shown an line. Press 'u' on the keyboard if the line DOES belong to the category, and 'i' if it DOES NOT belong to the category.\n\n",
        font='Open Sans',
        pos=[0,0], draggable=False, height=20.0, wrapWidth=None, ori=0.0, 
        color='black', colorSpace='rgb', opacity=None, 
        languageStyle='LTR',
        depth=-2.0);
    instr3_4 = visual.TextStim(win=win, name='instr3_4',
        text="Press 'space' to continue the instructions\n",
        font='Open Sans',
        pos=[0,-100], draggable=False, height=20.0, wrapWidth=None, ori=0.0, 
        color='black', colorSpace='rgb', opacity=None, 
        languageStyle='LTR',
        depth=-3.0);
    instr_cont3 = keyboard.Keyboard(deviceName='instr_cont3')
    fixation_cross = visual.ShapeStim(
        win=win, name='fixation_cross', vertices='cross',
        size=(5, 5),
        ori=0.0, pos=(0, 0), draggable=False, anchor='center',
        lineWidth=2.0,
        colorSpace='rgb', lineColor='black', fillColor='black',
        opacity=None, depth=-5.0, interpolate=True)
    
    # --- Initialize components for Routine "instructions4" ---
    instr4_1 = visual.TextStim(win=win, name='instr4_1',
        text='At first you will be guessing. However, after each trial, you will be told whether or not your answer was correct and whether or not the line belongs in the category. This feedback will allow you to learn about the category.\n',
        font='Open Sans',
        pos=[0,400], draggable=False, height=20.0, wrapWidth=None, ori=0.0, 
        color='black', colorSpace='rgb', opacity=None, 
        languageStyle='LTR',
        depth=0.0);
    instr4_2 = visual.TextStim(win=win, name='instr4_2',
        text="Don't get discouraged! Like we said, there is uncertainty in this category. Your job is not to perform perfectly, but to learn which types of stimuli are more likely to be in the category than others.",
        font='Open Sans',
        pos=[0,200], draggable=False, height=20.0, wrapWidth=None, ori=0.0, 
        color='black', colorSpace='rgb', opacity=None, 
        languageStyle='LTR',
        depth=-1.0);
    instr4_3 = visual.TextStim(win=win, name='instr4_3',
        text='With that being said, please try do your best.',
        font='Open Sans',
        pos=[0,0], draggable=False, height=20.0, wrapWidth=None, ori=0.0, 
        color='black', colorSpace='rgb', opacity=None, 
        languageStyle='LTR',
        depth=-2.0);
    instr4_4 = visual.TextStim(win=win, name='instr4_4',
        text="Press 'space' to continue the instructions.",
        font='Open Sans',
        pos=[0,-100], draggable=False, height=20.0, wrapWidth=None, ori=0.0, 
        color='black', colorSpace='rgb', opacity=None, 
        languageStyle='LTR',
        depth=-3.0);
    instr_cont4 = keyboard.Keyboard(deviceName='instr_cont4')
    
    # --- Initialize components for Routine "instructions5" ---
    instr5_1 = visual.TextStim(win=win, name='instr5_1',
        text="There will be another section in which you will use what you've learned to make more categorizations - but instructions for that will come later.",
        font='Open Sans',
        pos=[0,400], draggable=False, height=20.0, wrapWidth=None, ori=0.0, 
        color='black', colorSpace='rgb', opacity=None, 
        languageStyle='LTR',
        depth=0.0);
    instr5_2 = visual.TextStim(win=win, name='instr5_2',
        text='If you have any questions, now would be a good time to ask the experimenter.',
        font='Open Sans',
        pos=[0,200], draggable=False, height=20.0, wrapWidth=None, ori=0.0, 
        color='black', colorSpace='rgb', opacity=None, 
        languageStyle='LTR',
        depth=-1.0);
    instr5_3 = visual.TextStim(win=win, name='instr5_3',
        text="Press 'space' to continue the instructions.",
        font='Open Sans',
        pos=[0,0], draggable=False, height=20.0, wrapWidth=None, ori=0.0, 
        color='black', colorSpace='rgb', opacity=None, 
        languageStyle='LTR',
        depth=-2.0);
    instr_cont5 = keyboard.Keyboard(deviceName='instr_cont5')
    
    # --- Initialize components for Routine "instructions6" ---
    instr6_1 = visual.TextStim(win=win, name='instr6_1',
        text="On the next screen, you will be shown examples of lines that belong to the category, and examples of lines that don't belong to the category.",
        font='Open Sans',
        pos=[0,400], draggable=False, height=20.0, wrapWidth=None, ori=0.0, 
        color='black', colorSpace='rgb', opacity=None, 
        languageStyle='LTR',
        depth=0.0);
    instr6_2 = visual.TextStim(win=win, name='instr6_2',
        text='Each set of lines will be on the screen for 30 seconds, and then the experiment will start. Please study these lines closely.',
        font='Open Sans',
        pos=[0,200], draggable=False, height=20.0, wrapWidth=None, ori=0.0, 
        color='black', colorSpace='rgb', opacity=None, 
        languageStyle='LTR',
        depth=-1.0);
    instr6_3 = visual.TextStim(win=win, name='instr6_3',
        text="Press 'space' to see examples of lines in the category.\n",
        font='Open Sans',
        pos=[0,0], draggable=False, height=20.0, wrapWidth=None, ori=0.0, 
        color='black', colorSpace='rgb', opacity=None, 
        languageStyle='LTR',
        depth=-2.0);
    instr_cont6 = keyboard.Keyboard(deviceName='instr_cont6')
    
    # --- Initialize components for Routine "read_in_demo_vals" ---
    
    # --- Initialize components for Routine "learn_demo_in" ---
    
    # --- Initialize components for Routine "learn_start" ---
    learn_start_text = visual.TextStim(win=win, name='learn_start_text',
        text="Press 'space' to start the experiment.",
        font='Open Sans',
        pos=(0, 0), draggable=False, height=20.0, wrapWidth=None, ori=0.0, 
        color='black', colorSpace='rgb', opacity=None, 
        languageStyle='LTR',
        depth=0.0);
    learn_start_reps = keyboard.Keyboard(deviceName='learn_start_reps')
    
    # --- Initialize components for Routine "learn_trial" ---
    line_learn = visual.Line(
        win=win, name='line_learn',units='pix', 
        size=[1.0, 1.0],
        ori=0.0, pos=[0,0], draggable=False, anchor='bottom-center',
        lineWidth=line_width,
        colorSpace='rgb', lineColor=[-1.0000, -1.0000, -1.0000], fillColor=[-1.0000, -1.0000, -1.0000],
        opacity=None, depth=0.0, interpolate=True)
    learn_prompt = visual.TextStim(win=win, name='learn_prompt',
        text='Does the line belong in the category?\n(U=YES, I=NO)',
        font='Open Sans',
        pos=(0,-100), draggable=False, height=20.0, wrapWidth=None, ori=0.0, 
        color='black', colorSpace='rgb', opacity=None, 
        languageStyle='LTR',
        depth=-1.0);
    key_resp = keyboard.Keyboard(deviceName='key_resp')
    
    # --- Initialize components for Routine "learn_feedback" ---
    learn_feedback_text_display = visual.TextStim(win=win, name='learn_feedback_text_display',
        text='',
        font='Open Sans',
        pos=(0,-100), draggable=False, height=20.0, wrapWidth=None, ori=0.0, 
        color='white', colorSpace='rgb', opacity=None, 
        languageStyle='LTR',
        depth=-1.0);
    line_for_feedback = visual.Line(
        win=win, name='line_for_feedback',units='pix', 
        size=[1.0, 1.0],
        ori=0.0, pos=[0,0], draggable=False, anchor='bottom-center',
        lineWidth=line_width,
        colorSpace='rgb', lineColor=[-1.0000, -1.0000, -1.0000], fillColor=[-1.0000, -1.0000, -1.0000],
        opacity=None, depth=-2.0, interpolate=True)
    
    # --- Initialize components for Routine "blank" ---
    blank_200 = visual.TextStim(win=win, name='blank_200',
        text=None,
        font='Open Sans',
        pos=(0, 0), draggable=False, height=0.05, wrapWidth=None, ori=0.0, 
        color='white', colorSpace='rgb', opacity=None, 
        languageStyle='LTR',
        depth=0.0);
    
    # --- Initialize components for Routine "transfer_instructions1" ---
    tinstr1_1 = visual.TextStim(win=win, name='tinstr1_1',
        text="The learning phase is now complete. Press 'space' to continue.",
        font='Open Sans',
        pos=(0, 0), draggable=False, height=20.0, wrapWidth=None, ori=0.0, 
        color='black', colorSpace='rgb', opacity=None, 
        languageStyle='LTR',
        depth=0.0);
    transfer_cont1 = keyboard.Keyboard(deviceName='transfer_cont1')
    
    # --- Initialize components for Routine "transfer_instructions2" ---
    tinstr2_1 = visual.TextStim(win=win, name='tinstr2_1',
        text="Using what you've learned",
        font='Open Sans',
        pos=(0, 400), draggable=False, height=35.0, wrapWidth=None, ori=0.0, 
        color='black', colorSpace='rgb', opacity=None, 
        languageStyle='LTR',
        depth=0.0);
    tinstr2_2 = visual.TextStim(win=win, name='tinstr2_2',
        text="Now that you have learned the category, we will ask you to use what you've learned to categorize some new lines. During these trials you will be presented with multiple lines and you will determine the line that BEST belongs to the category.",
        font='Open Sans',
        pos=(0, 200), draggable=False, height=20.0, wrapWidth=None, ori=0.0, 
        color='black', colorSpace='rgb', opacity=None, 
        languageStyle='LTR',
        depth=-1.0);
    tinstr2_3 = visual.TextStim(win=win, name='tinstr2_3',
        text='The category is the SAME as you learned in training. The only difference is now you are choosing between multiple options, rather than just one. You will still receive feedback after each choice.',
        font='Open Sans',
        pos=(0, 0), draggable=False, height=20.0, wrapWidth=None, ori=0.0, 
        color='black', colorSpace='rgb', opacity=None, 
        languageStyle='LTR',
        depth=-2.0);
    tinstr2_4 = visual.TextStim(win=win, name='tinstr2_4',
        text='Please remember to try your best.',
        font='Open Sans',
        pos=(0, -100), draggable=False, height=20.0, wrapWidth=None, ori=0.0, 
        color='black', colorSpace='rgb', opacity=None, 
        languageStyle='LTR',
        depth=-3.0);
    tinstr2_5 = visual.TextStim(win=win, name='tinstr2_5',
        text="Thank you, and good luck!\nPress 'space' to continue\n",
        font='Open Sans',
        pos=(0, -200), draggable=False, height=20.0, wrapWidth=None, ori=0.0, 
        color='black', colorSpace='rgb', opacity=None, 
        languageStyle='LTR',
        depth=-4.0);
    transfer_cont2 = keyboard.Keyboard(deviceName='transfer_cont2')
    
    # --- Initialize components for Routine "transfer_instructions3" ---
    tinstr3_1 = visual.TextStim(win=win, name='tinstr3_1',
        text="Press 'space' to continue to the next phase of the experiment.",
        font='Open Sans',
        pos=(0, -100), draggable=False, height=20.0, wrapWidth=None, ori=0.0, 
        color='black', colorSpace='rgb', opacity=None, 
        languageStyle='LTR',
        depth=0.0);
    transfer_cont3 = keyboard.Keyboard(deviceName='transfer_cont3')
    
    # --- Initialize components for Routine "shuffle_trial_stimuli" ---
    
    # --- Initialize components for Routine "transfer_trial_trinary" ---
    line_1_trinary = visual.Line(
        win=win, name='line_1_trinary',
        size=[1.0, 1.0],
        ori=0.0, pos=[0,0], draggable=False, anchor='bottom-center',
        lineWidth=line_width,
        colorSpace='rgb', lineColor='black', fillColor='black',
        opacity=None, depth=0.0, interpolate=True)
    line_2_trinary = visual.Line(
        win=win, name='line_2_trinary',
        size=[1.0, 1.0],
        ori=0.0, pos=[0,0], draggable=False, anchor='bottom-center',
        lineWidth=line_width,
        colorSpace='rgb', lineColor='black', fillColor='black',
        opacity=None, depth=-1.0, interpolate=True)
    line_3_trinary = visual.Line(
        win=win, name='line_3_trinary',
        size=[1.0, 1.0],
        ori=0.0, pos=[0,0], draggable=False, anchor='bottom-center',
        lineWidth=line_width,
        colorSpace='rgb', lineColor='black', fillColor='black',
        opacity=None, depth=-2.0, interpolate=True)
    key_resp_transfer_trinary = keyboard.Keyboard(deviceName='key_resp_transfer_trinary')
    line_1_trinary_label = visual.TextStim(win=win, name='line_1_trinary_label',
        text='J',
        font='Open Sans',
        pos=(-dist_btw, -30), draggable=False, height=20.0, wrapWidth=None, ori=0.0, 
        color='black', colorSpace='rgb', opacity=None, 
        languageStyle='LTR',
        depth=-4.0);
    line_2_trinary_label = visual.TextStim(win=win, name='line_2_trinary_label',
        text='K',
        font='Open Sans',
        pos=(0, -30), draggable=False, height=20.0, wrapWidth=None, ori=0.0, 
        color='black', colorSpace='rgb', opacity=None, 
        languageStyle='LTR',
        depth=-5.0);
    line_3_trinary_label = visual.TextStim(win=win, name='line_3_trinary_label',
        text='L',
        font='Open Sans',
        pos=(dist_btw, -30), draggable=False, height=20.0, wrapWidth=None, ori=0.0, 
        color='black', colorSpace='rgb', opacity=None, 
        languageStyle='LTR',
        depth=-6.0);
    transfer_trinary_prompt = visual.TextStim(win=win, name='transfer_trinary_prompt',
        text='Which line is most likely to belong in the category?\nJ or K?',
        font='Open Sans',
        pos=(0,-150), draggable=False, height=20.0, wrapWidth=None, ori=0.0, 
        color='black', colorSpace='rgb', opacity=None, 
        languageStyle='LTR',
        depth=-7.0);
    
    # --- Initialize components for Routine "transfer_trinary_feedback" ---
    transfer_trinary_feedback_text_display = visual.TextStim(win=win, name='transfer_trinary_feedback_text_display',
        text='',
        font='Open Sans',
        pos=[0,0], draggable=False, height=20.0, wrapWidth=None, ori=0.0, 
        color='white', colorSpace='rgb', opacity=None, 
        languageStyle='LTR',
        depth=-1.0);
    
    # --- Initialize components for Routine "blank" ---
    blank_200 = visual.TextStim(win=win, name='blank_200',
        text=None,
        font='Open Sans',
        pos=(0, 0), draggable=False, height=0.05, wrapWidth=None, ori=0.0, 
        color='white', colorSpace='rgb', opacity=None, 
        languageStyle='LTR',
        depth=0.0);
    
    # --- Initialize components for Routine "transfer_trial_binary" ---
    line_1_binary = visual.Line(
        win=win, name='line_1_binary',
        size=[1.0, 1.0],
        ori=0.0, pos=[0,0], draggable=False, anchor='bottom-center',
        lineWidth=line_width,
        colorSpace='rgb', lineColor='black', fillColor='black',
        opacity=None, depth=0.0, interpolate=True)
    line_2_binary = visual.Line(
        win=win, name='line_2_binary',
        size=[1.0, 1.0],
        ori=0.0, pos=[0,0], draggable=False, anchor='bottom-center',
        lineWidth=line_width,
        colorSpace='rgb', lineColor='black', fillColor='black',
        opacity=None, depth=-1.0, interpolate=True)
    line_1_label_binary = visual.TextStim(win=win, name='line_1_label_binary',
        text='J',
        font='Open Sans',
        pos=(-dist_btw/2, -30), draggable=False, height=20.0, wrapWidth=None, ori=0.0, 
        color=[-1.0000, -1.0000, -1.0000], colorSpace='rgb', opacity=None, 
        languageStyle='LTR',
        depth=-2.0);
    line_2_label_binary = visual.TextStim(win=win, name='line_2_label_binary',
        text='K',
        font='Open Sans',
        pos=(dist_btw/2, -30), draggable=False, height=20.0, wrapWidth=None, ori=0.0, 
        color=[-1.0000, -1.0000, -1.0000], colorSpace='rgb', opacity=None, 
        languageStyle='LTR',
        depth=-3.0);
    transfer_binary_prompt = visual.TextStim(win=win, name='transfer_binary_prompt',
        text='Which line is most likely to belong in the category?\nJ or K?',
        font='Open Sans',
        pos=(0,-150), draggable=False, height=20.0, wrapWidth=None, ori=0.0, 
        color='black', colorSpace='rgb', opacity=None, 
        languageStyle='LTR',
        depth=-4.0);
    key_resp_transfer_binary = keyboard.Keyboard(deviceName='key_resp_transfer_binary')
    
    # --- Initialize components for Routine "transfer_binary_feedback" ---
    transfer_binary_feedback_text_display = visual.TextStim(win=win, name='transfer_binary_feedback_text_display',
        text='',
        font='Open Sans',
        pos=(0,-150), draggable=False, height=20.0, wrapWidth=None, ori=0.0, 
        color='white', colorSpace='rgb', opacity=None, 
        languageStyle='LTR',
        depth=-1.0);
    
    # --- Initialize components for Routine "blank" ---
    blank_200 = visual.TextStim(win=win, name='blank_200',
        text=None,
        font='Open Sans',
        pos=(0, 0), draggable=False, height=0.05, wrapWidth=None, ori=0.0, 
        color='white', colorSpace='rgb', opacity=None, 
        languageStyle='LTR',
        depth=0.0);
    
    # --- Initialize components for Routine "exp_finished" ---
    exp_finished_txt1 = visual.TextStim(win=win, name='exp_finished_txt1',
        text='You have now reached the end of the experiment. Thank you for participating!',
        font='Open Sans',
        pos=(0, 400), draggable=False, height=20.0, wrapWidth=None, ori=0.0, 
        color='black', colorSpace='rgb', opacity=None, 
        languageStyle='LTR',
        depth=0.0);
    exp_finished_txt2 = visual.TextStim(win=win, name='exp_finished_txt2',
        text="On the next page is the debriefing form. You are welcome to read it for as long as you wish. Press 'space' when you have finished reading. This will close out the experiment, and you can let the researcher know that you are finished.",
        font='Open Sans',
        pos=(0, 200), draggable=False, height=20.0, wrapWidth=None, ori=0.0, 
        color='black', colorSpace='rgb', opacity=None, 
        languageStyle='LTR',
        depth=-1.0);
    exp_finished_txt3 = visual.TextStim(win=win, name='exp_finished_txt3',
        text="Press 'space' to read the debriefing form.",
        font='Open Sans',
        pos=[0,0], draggable=False, height=20.0, wrapWidth=None, ori=0.0, 
        color='white', colorSpace='rgb', opacity=None, 
        languageStyle='LTR',
        depth=-2.0);
    cont_to_debrief = keyboard.Keyboard(deviceName='cont_to_debrief')
    
    # --- Initialize components for Routine "debriefing" ---
    debrief_form = visual.ImageStim(
        win=win,
        name='debrief_form', 
        image='Judg_choice_debrief.png', mask=None, anchor='bottom-center',
        ori=0.0, pos=(0, -500), draggable=False, size=(800, 1000),
        color=[1,1,1], colorSpace='rgb', opacity=None,
        flipHoriz=False, flipVert=False,
        texRes=128.0, interpolate=True, depth=0.0)
    end_debrief = keyboard.Keyboard(deviceName='end_debrief')
    # Run 'Begin Experiment' code from log_import
    from psychopy import logging
    
    # create some handy timers
    
    # global clock to track the time since experiment started
    if globalClock is None:
        # create a clock if not given one
        globalClock = core.Clock()
    if isinstance(globalClock, str):
        # if given a string, make a clock accoridng to it
        if globalClock == 'float':
            # get timestamps as a simple value
            globalClock = core.Clock(format='float')
        elif globalClock == 'iso':
            # get timestamps in ISO format
            globalClock = core.Clock(format='%Y-%m-%d_%H:%M:%S.%f%z')
        else:
            # get timestamps in a custom format
            globalClock = core.Clock(format=globalClock)
    if ioServer is not None:
        ioServer.syncClock(globalClock)
    logging.setDefaultClock(globalClock)
    # routine timer to track time remaining of each (possibly non-slip) routine
    routineTimer = core.Clock()
    win.flip()  # flip window to reset last flip timer
    # store the exact time the global clock started
    expInfo['expStart'] = data.getDateStr(
        format='%Y-%m-%d %Hh%M.%S.%f %z', fractionalSecondDigits=6
    )
    
    # --- Prepare to start Routine "consent" ---
    # create an object to store info about Routine consent
    consent = data.Routine(
        name='consent',
        components=[agree_button, decline_button, consent_img, mouse],
    )
    consent.status = NOT_STARTED
    continueRoutine = True
    # update component parameters for each repeat
    # reset agree_button to account for continued clicks & clear times on/off
    agree_button.reset()
    # reset decline_button to account for continued clicks & clear times on/off
    decline_button.reset()
    # setup some python lists for storing info about the mouse
    mouse.x = []
    mouse.y = []
    mouse.leftButton = []
    mouse.midButton = []
    mouse.rightButton = []
    mouse.time = []
    mouse.clicked_name = []
    gotValidClick = False  # until a click is received
    # store start times for consent
    consent.tStartRefresh = win.getFutureFlipTime(clock=globalClock)
    consent.tStart = globalClock.getTime(format='float')
    consent.status = STARTED
    thisExp.addData('consent.started', consent.tStart)
    consent.maxDuration = None
    # keep track of which components have finished
    consentComponents = consent.components
    for thisComponent in consent.components:
        thisComponent.tStart = None
        thisComponent.tStop = None
        thisComponent.tStartRefresh = None
        thisComponent.tStopRefresh = None
        if hasattr(thisComponent, 'status'):
            thisComponent.status = NOT_STARTED
    # reset timers
    t = 0
    _timeToFirstFrame = win.getFutureFlipTime(clock="now")
    frameN = -1
    
    # --- Run Routine "consent" ---
    consent.forceEnded = routineForceEnded = not continueRoutine
    while continueRoutine:
        # get current time
        t = routineTimer.getTime()
        tThisFlip = win.getFutureFlipTime(clock=routineTimer)
        tThisFlipGlobal = win.getFutureFlipTime(clock=None)
        frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
        # update/draw components on each frame
        # *agree_button* updates
        
        # if agree_button is starting this frame...
        if agree_button.status == NOT_STARTED and tThisFlip >= 0-frameTolerance:
            # keep track of start time/frame for later
            agree_button.frameNStart = frameN  # exact frame index
            agree_button.tStart = t  # local t and not account for scr refresh
            agree_button.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(agree_button, 'tStartRefresh')  # time at next scr refresh
            # add timestamp to datafile
            thisExp.timestampOnFlip(win, 'agree_button.started')
            # update status
            agree_button.status = STARTED
            win.callOnFlip(agree_button.buttonClock.reset)
            agree_button.setAutoDraw(True)
        
        # if agree_button is active this frame...
        if agree_button.status == STARTED:
            # update params
            pass
            # check whether agree_button has been pressed
            if agree_button.isClicked:
                if not agree_button.wasClicked:
                    # if this is a new click, store time of first click and clicked until
                    agree_button.timesOn.append(agree_button.buttonClock.getTime())
                    agree_button.timesOff.append(agree_button.buttonClock.getTime())
                elif len(agree_button.timesOff):
                    # if click is continuing from last frame, update time of clicked until
                    agree_button.timesOff[-1] = agree_button.buttonClock.getTime()
                if not agree_button.wasClicked:
                    # end routine when agree_button is clicked
                    continueRoutine = False
                if not agree_button.wasClicked:
                    # run callback code when agree_button is clicked
                    pass
        # take note of whether agree_button was clicked, so that next frame we know if clicks are new
        agree_button.wasClicked = agree_button.isClicked and agree_button.status == STARTED
        # *decline_button* updates
        
        # if decline_button is starting this frame...
        if decline_button.status == NOT_STARTED and tThisFlip >= 0-frameTolerance:
            # keep track of start time/frame for later
            decline_button.frameNStart = frameN  # exact frame index
            decline_button.tStart = t  # local t and not account for scr refresh
            decline_button.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(decline_button, 'tStartRefresh')  # time at next scr refresh
            # add timestamp to datafile
            thisExp.timestampOnFlip(win, 'decline_button.started')
            # update status
            decline_button.status = STARTED
            win.callOnFlip(decline_button.buttonClock.reset)
            decline_button.setAutoDraw(True)
        
        # if decline_button is active this frame...
        if decline_button.status == STARTED:
            # update params
            pass
            # check whether decline_button has been pressed
            if decline_button.isClicked:
                if not decline_button.wasClicked:
                    # if this is a new click, store time of first click and clicked until
                    decline_button.timesOn.append(decline_button.buttonClock.getTime())
                    decline_button.timesOff.append(decline_button.buttonClock.getTime())
                elif len(decline_button.timesOff):
                    # if click is continuing from last frame, update time of clicked until
                    decline_button.timesOff[-1] = decline_button.buttonClock.getTime()
                if not decline_button.wasClicked:
                    # end routine when decline_button is clicked
                    continueRoutine = False
                if not decline_button.wasClicked:
                    # run callback code when decline_button is clicked
                    pass
        # take note of whether decline_button was clicked, so that next frame we know if clicks are new
        decline_button.wasClicked = decline_button.isClicked and decline_button.status == STARTED
        
        # *consent_img* updates
        
        # if consent_img is starting this frame...
        if consent_img.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            consent_img.frameNStart = frameN  # exact frame index
            consent_img.tStart = t  # local t and not account for scr refresh
            consent_img.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(consent_img, 'tStartRefresh')  # time at next scr refresh
            # add timestamp to datafile
            thisExp.timestampOnFlip(win, 'consent_img.started')
            # update status
            consent_img.status = STARTED
            consent_img.setAutoDraw(True)
        
        # if consent_img is active this frame...
        if consent_img.status == STARTED:
            # update params
            pass
        # *mouse* updates
        
        # if mouse is starting this frame...
        if mouse.status == NOT_STARTED and t >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            mouse.frameNStart = frameN  # exact frame index
            mouse.tStart = t  # local t and not account for scr refresh
            mouse.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(mouse, 'tStartRefresh')  # time at next scr refresh
            # add timestamp to datafile
            thisExp.addData('mouse.started', t)
            # update status
            mouse.status = STARTED
            mouse.mouseClock.reset()
            prevButtonState = mouse.getPressed()  # if button is down already this ISN'T a new click
        if mouse.status == STARTED:  # only update if started and not finished!
            buttons = mouse.getPressed()
            if buttons != prevButtonState:  # button state changed?
                prevButtonState = buttons
                if sum(buttons) > 0:  # state changed to a new click
                    # check if the mouse was inside our 'clickable' objects
                    gotValidClick = False
                    clickableList = environmenttools.getFromNames([agree_button, decline_button], namespace=locals())
                    for obj in clickableList:
                        # is this object clicked on?
                        if obj.contains(mouse):
                            gotValidClick = True
                            mouse.clicked_name.append(obj.name)
                    if not gotValidClick:
                        mouse.clicked_name.append(None)
                    x, y = mouse.getPos()
                    mouse.x.append(x)
                    mouse.y.append(y)
                    buttons = mouse.getPressed()
                    mouse.leftButton.append(buttons[0])
                    mouse.midButton.append(buttons[1])
                    mouse.rightButton.append(buttons[2])
                    mouse.time.append(mouse.mouseClock.getTime())
                    if gotValidClick:
                        continueRoutine = False  # end routine on response
        
        # check for quit (typically the Esc key)
        if defaultKeyboard.getKeys(keyList=["escape"]):
            thisExp.status = FINISHED
        if thisExp.status == FINISHED or endExpNow:
            endExperiment(thisExp, win=win)
            return
        # pause experiment here if requested
        if thisExp.status == PAUSED:
            pauseExperiment(
                thisExp=thisExp, 
                win=win, 
                timers=[routineTimer], 
                playbackComponents=[]
            )
            # skip the frame we paused on
            continue
        
        # check if all components have finished
        if not continueRoutine:  # a component has requested a forced-end of Routine
            consent.forceEnded = routineForceEnded = True
            break
        continueRoutine = False  # will revert to True if at least one component still running
        for thisComponent in consent.components:
            if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                continueRoutine = True
                break  # at least one component has not yet finished
        
        # refresh the screen
        if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
            win.flip()
    
    # --- Ending Routine "consent" ---
    for thisComponent in consent.components:
        if hasattr(thisComponent, "setAutoDraw"):
            thisComponent.setAutoDraw(False)
    # store stop times for consent
    consent.tStop = globalClock.getTime(format='float')
    consent.tStopRefresh = tThisFlipGlobal
    thisExp.addData('consent.stopped', consent.tStop)
    thisExp.addData('agree_button.numClicks', agree_button.numClicks)
    if agree_button.numClicks:
       thisExp.addData('agree_button.timesOn', agree_button.timesOn)
       thisExp.addData('agree_button.timesOff', agree_button.timesOff)
    else:
       thisExp.addData('agree_button.timesOn', "")
       thisExp.addData('agree_button.timesOff', "")
    thisExp.addData('decline_button.numClicks', decline_button.numClicks)
    if decline_button.numClicks:
       thisExp.addData('decline_button.timesOn', decline_button.timesOn)
       thisExp.addData('decline_button.timesOff', decline_button.timesOff)
    else:
       thisExp.addData('decline_button.timesOn', "")
       thisExp.addData('decline_button.timesOff', "")
    # Run 'End Routine' code from check_consent
    if decline_button.numClicks==1:
        core.quit()
    # store data for thisExp (ExperimentHandler)
    thisExp.addData('mouse.x', mouse.x)
    thisExp.addData('mouse.y', mouse.y)
    thisExp.addData('mouse.leftButton', mouse.leftButton)
    thisExp.addData('mouse.midButton', mouse.midButton)
    thisExp.addData('mouse.rightButton', mouse.rightButton)
    thisExp.addData('mouse.time', mouse.time)
    thisExp.addData('mouse.clicked_name', mouse.clicked_name)
    thisExp.nextEntry()
    # the Routine "consent" was not non-slip safe, so reset the non-slip timer
    routineTimer.reset()
    
    # --- Prepare to start Routine "exp_setup" ---
    # create an object to store info about Routine exp_setup
    exp_setup = data.Routine(
        name='exp_setup',
        components=[],
    )
    exp_setup.status = NOT_STARTED
    continueRoutine = True
    # update component parameters for each repeat
    # store start times for exp_setup
    exp_setup.tStartRefresh = win.getFutureFlipTime(clock=globalClock)
    exp_setup.tStart = globalClock.getTime(format='float')
    exp_setup.status = STARTED
    thisExp.addData('exp_setup.started', exp_setup.tStart)
    exp_setup.maxDuration = None
    # keep track of which components have finished
    exp_setupComponents = exp_setup.components
    for thisComponent in exp_setup.components:
        thisComponent.tStart = None
        thisComponent.tStop = None
        thisComponent.tStartRefresh = None
        thisComponent.tStopRefresh = None
        if hasattr(thisComponent, 'status'):
            thisComponent.status = NOT_STARTED
    # reset timers
    t = 0
    _timeToFirstFrame = win.getFutureFlipTime(clock="now")
    frameN = -1
    
    # --- Run Routine "exp_setup" ---
    exp_setup.forceEnded = routineForceEnded = not continueRoutine
    while continueRoutine:
        # get current time
        t = routineTimer.getTime()
        tThisFlip = win.getFutureFlipTime(clock=routineTimer)
        tThisFlipGlobal = win.getFutureFlipTime(clock=None)
        frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
        # update/draw components on each frame
        
        # check for quit (typically the Esc key)
        if defaultKeyboard.getKeys(keyList=["escape"]):
            thisExp.status = FINISHED
        if thisExp.status == FINISHED or endExpNow:
            endExperiment(thisExp, win=win)
            return
        # pause experiment here if requested
        if thisExp.status == PAUSED:
            pauseExperiment(
                thisExp=thisExp, 
                win=win, 
                timers=[routineTimer], 
                playbackComponents=[]
            )
            # skip the frame we paused on
            continue
        
        # check if all components have finished
        if not continueRoutine:  # a component has requested a forced-end of Routine
            exp_setup.forceEnded = routineForceEnded = True
            break
        continueRoutine = False  # will revert to True if at least one component still running
        for thisComponent in exp_setup.components:
            if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                continueRoutine = True
                break  # at least one component has not yet finished
        
        # refresh the screen
        if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
            win.flip()
    
    # --- Ending Routine "exp_setup" ---
    for thisComponent in exp_setup.components:
        if hasattr(thisComponent, "setAutoDraw"):
            thisComponent.setAutoDraw(False)
    # store stop times for exp_setup
    exp_setup.tStop = globalClock.getTime(format='float')
    exp_setup.tStopRefresh = tThisFlipGlobal
    thisExp.addData('exp_setup.stopped', exp_setup.tStop)
    # Run 'End Routine' code from set_condition_in_data
    thisExp.addData('exp_condition',exp_condition)
    thisExp.nextEntry()
    # the Routine "exp_setup" was not non-slip safe, so reset the non-slip timer
    routineTimer.reset()
    
    # set up handler to look after randomisation of conditions etc
    instructions_loop = data.TrialHandler2(
        name='instructions_loop',
        nReps=0.0, 
        method='sequential', 
        extraInfo=expInfo, 
        originPath=-1, 
        trialList=[None], 
        seed=None, 
    )
    thisExp.addLoop(instructions_loop)  # add the loop to the experiment
    thisInstructions_loop = instructions_loop.trialList[0]  # so we can initialise stimuli with some values
    # abbreviate parameter names if possible (e.g. rgb = thisInstructions_loop.rgb)
    if thisInstructions_loop != None:
        for paramName in thisInstructions_loop:
            globals()[paramName] = thisInstructions_loop[paramName]
    
    for thisInstructions_loop in instructions_loop:
        currentLoop = instructions_loop
        thisExp.timestampOnFlip(win, 'thisRow.t', format=globalClock.format)
        # abbreviate parameter names if possible (e.g. rgb = thisInstructions_loop.rgb)
        if thisInstructions_loop != None:
            for paramName in thisInstructions_loop:
                globals()[paramName] = thisInstructions_loop[paramName]
        
        # --- Prepare to start Routine "instructions1" ---
        # create an object to store info about Routine instructions1
        instructions1 = data.Routine(
            name='instructions1',
            components=[instr1_1, instr_1_2, instr1_line1, instr1_line2, instr_cont1, instr_1_3],
        )
        instructions1.status = NOT_STARTED
        continueRoutine = True
        # update component parameters for each repeat
        # create starting attributes for instr_cont1
        instr_cont1.keys = []
        instr_cont1.rt = []
        _instr_cont1_allKeys = []
        # store start times for instructions1
        instructions1.tStartRefresh = win.getFutureFlipTime(clock=globalClock)
        instructions1.tStart = globalClock.getTime(format='float')
        instructions1.status = STARTED
        thisExp.addData('instructions1.started', instructions1.tStart)
        instructions1.maxDuration = None
        # keep track of which components have finished
        instructions1Components = instructions1.components
        for thisComponent in instructions1.components:
            thisComponent.tStart = None
            thisComponent.tStop = None
            thisComponent.tStartRefresh = None
            thisComponent.tStopRefresh = None
            if hasattr(thisComponent, 'status'):
                thisComponent.status = NOT_STARTED
        # reset timers
        t = 0
        _timeToFirstFrame = win.getFutureFlipTime(clock="now")
        frameN = -1
        
        # --- Run Routine "instructions1" ---
        # if trial has changed, end Routine now
        if isinstance(instructions_loop, data.TrialHandler2) and thisInstructions_loop.thisN != instructions_loop.thisTrial.thisN:
            continueRoutine = False
        instructions1.forceEnded = routineForceEnded = not continueRoutine
        while continueRoutine:
            # get current time
            t = routineTimer.getTime()
            tThisFlip = win.getFutureFlipTime(clock=routineTimer)
            tThisFlipGlobal = win.getFutureFlipTime(clock=None)
            frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
            # update/draw components on each frame
            
            # *instr1_1* updates
            
            # if instr1_1 is starting this frame...
            if instr1_1.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                # keep track of start time/frame for later
                instr1_1.frameNStart = frameN  # exact frame index
                instr1_1.tStart = t  # local t and not account for scr refresh
                instr1_1.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(instr1_1, 'tStartRefresh')  # time at next scr refresh
                # add timestamp to datafile
                thisExp.timestampOnFlip(win, 'instr1_1.started')
                # update status
                instr1_1.status = STARTED
                instr1_1.setAutoDraw(True)
            
            # if instr1_1 is active this frame...
            if instr1_1.status == STARTED:
                # update params
                pass
            
            # *instr_1_2* updates
            
            # if instr_1_2 is starting this frame...
            if instr_1_2.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                # keep track of start time/frame for later
                instr_1_2.frameNStart = frameN  # exact frame index
                instr_1_2.tStart = t  # local t and not account for scr refresh
                instr_1_2.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(instr_1_2, 'tStartRefresh')  # time at next scr refresh
                # add timestamp to datafile
                thisExp.timestampOnFlip(win, 'instr_1_2.started')
                # update status
                instr_1_2.status = STARTED
                instr_1_2.setAutoDraw(True)
            
            # if instr_1_2 is active this frame...
            if instr_1_2.status == STARTED:
                # update params
                pass
            
            # *instr1_line1* updates
            
            # if instr1_line1 is starting this frame...
            if instr1_line1.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                # keep track of start time/frame for later
                instr1_line1.frameNStart = frameN  # exact frame index
                instr1_line1.tStart = t  # local t and not account for scr refresh
                instr1_line1.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(instr1_line1, 'tStartRefresh')  # time at next scr refresh
                # add timestamp to datafile
                thisExp.timestampOnFlip(win, 'instr1_line1.started')
                # update status
                instr1_line1.status = STARTED
                instr1_line1.setAutoDraw(True)
            
            # if instr1_line1 is active this frame...
            if instr1_line1.status == STARTED:
                # update params
                pass
            
            # *instr1_line2* updates
            
            # if instr1_line2 is starting this frame...
            if instr1_line2.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                # keep track of start time/frame for later
                instr1_line2.frameNStart = frameN  # exact frame index
                instr1_line2.tStart = t  # local t and not account for scr refresh
                instr1_line2.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(instr1_line2, 'tStartRefresh')  # time at next scr refresh
                # add timestamp to datafile
                thisExp.timestampOnFlip(win, 'instr1_line2.started')
                # update status
                instr1_line2.status = STARTED
                instr1_line2.setAutoDraw(True)
            
            # if instr1_line2 is active this frame...
            if instr1_line2.status == STARTED:
                # update params
                pass
            
            # *instr_cont1* updates
            waitOnFlip = False
            
            # if instr_cont1 is starting this frame...
            if instr_cont1.status == NOT_STARTED and tThisFlip >= 3.0-frameTolerance:
                # keep track of start time/frame for later
                instr_cont1.frameNStart = frameN  # exact frame index
                instr_cont1.tStart = t  # local t and not account for scr refresh
                instr_cont1.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(instr_cont1, 'tStartRefresh')  # time at next scr refresh
                # add timestamp to datafile
                thisExp.timestampOnFlip(win, 'instr_cont1.started')
                # update status
                instr_cont1.status = STARTED
                # keyboard checking is just starting
                waitOnFlip = True
                win.callOnFlip(instr_cont1.clock.reset)  # t=0 on next screen flip
                win.callOnFlip(instr_cont1.clearEvents, eventType='keyboard')  # clear events on next screen flip
            if instr_cont1.status == STARTED and not waitOnFlip:
                theseKeys = instr_cont1.getKeys(keyList=['space'], ignoreKeys=["escape"], waitRelease=False)
                _instr_cont1_allKeys.extend(theseKeys)
                if len(_instr_cont1_allKeys):
                    instr_cont1.keys = _instr_cont1_allKeys[-1].name  # just the last key pressed
                    instr_cont1.rt = _instr_cont1_allKeys[-1].rt
                    instr_cont1.duration = _instr_cont1_allKeys[-1].duration
                    # a response ends the routine
                    continueRoutine = False
            
            # *instr_1_3* updates
            
            # if instr_1_3 is starting this frame...
            if instr_1_3.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                # keep track of start time/frame for later
                instr_1_3.frameNStart = frameN  # exact frame index
                instr_1_3.tStart = t  # local t and not account for scr refresh
                instr_1_3.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(instr_1_3, 'tStartRefresh')  # time at next scr refresh
                # add timestamp to datafile
                thisExp.timestampOnFlip(win, 'instr_1_3.started')
                # update status
                instr_1_3.status = STARTED
                instr_1_3.setAutoDraw(True)
            
            # if instr_1_3 is active this frame...
            if instr_1_3.status == STARTED:
                # update params
                pass
            
            # check for quit (typically the Esc key)
            if defaultKeyboard.getKeys(keyList=["escape"]):
                thisExp.status = FINISHED
            if thisExp.status == FINISHED or endExpNow:
                endExperiment(thisExp, win=win)
                return
            # pause experiment here if requested
            if thisExp.status == PAUSED:
                pauseExperiment(
                    thisExp=thisExp, 
                    win=win, 
                    timers=[routineTimer], 
                    playbackComponents=[]
                )
                # skip the frame we paused on
                continue
            
            # check if all components have finished
            if not continueRoutine:  # a component has requested a forced-end of Routine
                instructions1.forceEnded = routineForceEnded = True
                break
            continueRoutine = False  # will revert to True if at least one component still running
            for thisComponent in instructions1.components:
                if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                    continueRoutine = True
                    break  # at least one component has not yet finished
            
            # refresh the screen
            if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
                win.flip()
        
        # --- Ending Routine "instructions1" ---
        for thisComponent in instructions1.components:
            if hasattr(thisComponent, "setAutoDraw"):
                thisComponent.setAutoDraw(False)
        # store stop times for instructions1
        instructions1.tStop = globalClock.getTime(format='float')
        instructions1.tStopRefresh = tThisFlipGlobal
        thisExp.addData('instructions1.stopped', instructions1.tStop)
        # check responses
        if instr_cont1.keys in ['', [], None]:  # No response was made
            instr_cont1.keys = None
        instructions_loop.addData('instr_cont1.keys',instr_cont1.keys)
        if instr_cont1.keys != None:  # we had a response
            instructions_loop.addData('instr_cont1.rt', instr_cont1.rt)
            instructions_loop.addData('instr_cont1.duration', instr_cont1.duration)
        # the Routine "instructions1" was not non-slip safe, so reset the non-slip timer
        routineTimer.reset()
        
        # --- Prepare to start Routine "instructions2" ---
        # create an object to store info about Routine instructions2
        instructions2 = data.Routine(
            name='instructions2',
            components=[instr2_1, instr2_2, instr2_3, instr2_4, instr_cont2],
        )
        instructions2.status = NOT_STARTED
        continueRoutine = True
        # update component parameters for each repeat
        # create starting attributes for instr_cont2
        instr_cont2.keys = []
        instr_cont2.rt = []
        _instr_cont2_allKeys = []
        # store start times for instructions2
        instructions2.tStartRefresh = win.getFutureFlipTime(clock=globalClock)
        instructions2.tStart = globalClock.getTime(format='float')
        instructions2.status = STARTED
        thisExp.addData('instructions2.started', instructions2.tStart)
        instructions2.maxDuration = None
        # keep track of which components have finished
        instructions2Components = instructions2.components
        for thisComponent in instructions2.components:
            thisComponent.tStart = None
            thisComponent.tStop = None
            thisComponent.tStartRefresh = None
            thisComponent.tStopRefresh = None
            if hasattr(thisComponent, 'status'):
                thisComponent.status = NOT_STARTED
        # reset timers
        t = 0
        _timeToFirstFrame = win.getFutureFlipTime(clock="now")
        frameN = -1
        
        # --- Run Routine "instructions2" ---
        # if trial has changed, end Routine now
        if isinstance(instructions_loop, data.TrialHandler2) and thisInstructions_loop.thisN != instructions_loop.thisTrial.thisN:
            continueRoutine = False
        instructions2.forceEnded = routineForceEnded = not continueRoutine
        while continueRoutine:
            # get current time
            t = routineTimer.getTime()
            tThisFlip = win.getFutureFlipTime(clock=routineTimer)
            tThisFlipGlobal = win.getFutureFlipTime(clock=None)
            frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
            # update/draw components on each frame
            
            # *instr2_1* updates
            
            # if instr2_1 is starting this frame...
            if instr2_1.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                # keep track of start time/frame for later
                instr2_1.frameNStart = frameN  # exact frame index
                instr2_1.tStart = t  # local t and not account for scr refresh
                instr2_1.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(instr2_1, 'tStartRefresh')  # time at next scr refresh
                # add timestamp to datafile
                thisExp.timestampOnFlip(win, 'instr2_1.started')
                # update status
                instr2_1.status = STARTED
                instr2_1.setAutoDraw(True)
            
            # if instr2_1 is active this frame...
            if instr2_1.status == STARTED:
                # update params
                pass
            
            # *instr2_2* updates
            
            # if instr2_2 is starting this frame...
            if instr2_2.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                # keep track of start time/frame for later
                instr2_2.frameNStart = frameN  # exact frame index
                instr2_2.tStart = t  # local t and not account for scr refresh
                instr2_2.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(instr2_2, 'tStartRefresh')  # time at next scr refresh
                # add timestamp to datafile
                thisExp.timestampOnFlip(win, 'instr2_2.started')
                # update status
                instr2_2.status = STARTED
                instr2_2.setAutoDraw(True)
            
            # if instr2_2 is active this frame...
            if instr2_2.status == STARTED:
                # update params
                pass
            
            # *instr2_3* updates
            
            # if instr2_3 is starting this frame...
            if instr2_3.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                # keep track of start time/frame for later
                instr2_3.frameNStart = frameN  # exact frame index
                instr2_3.tStart = t  # local t and not account for scr refresh
                instr2_3.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(instr2_3, 'tStartRefresh')  # time at next scr refresh
                # add timestamp to datafile
                thisExp.timestampOnFlip(win, 'instr2_3.started')
                # update status
                instr2_3.status = STARTED
                instr2_3.setAutoDraw(True)
            
            # if instr2_3 is active this frame...
            if instr2_3.status == STARTED:
                # update params
                pass
            
            # *instr2_4* updates
            
            # if instr2_4 is starting this frame...
            if instr2_4.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                # keep track of start time/frame for later
                instr2_4.frameNStart = frameN  # exact frame index
                instr2_4.tStart = t  # local t and not account for scr refresh
                instr2_4.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(instr2_4, 'tStartRefresh')  # time at next scr refresh
                # add timestamp to datafile
                thisExp.timestampOnFlip(win, 'instr2_4.started')
                # update status
                instr2_4.status = STARTED
                instr2_4.setAutoDraw(True)
            
            # if instr2_4 is active this frame...
            if instr2_4.status == STARTED:
                # update params
                pass
            
            # *instr_cont2* updates
            waitOnFlip = False
            
            # if instr_cont2 is starting this frame...
            if instr_cont2.status == NOT_STARTED and tThisFlip >= 3.0-frameTolerance:
                # keep track of start time/frame for later
                instr_cont2.frameNStart = frameN  # exact frame index
                instr_cont2.tStart = t  # local t and not account for scr refresh
                instr_cont2.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(instr_cont2, 'tStartRefresh')  # time at next scr refresh
                # add timestamp to datafile
                thisExp.timestampOnFlip(win, 'instr_cont2.started')
                # update status
                instr_cont2.status = STARTED
                # keyboard checking is just starting
                waitOnFlip = True
                win.callOnFlip(instr_cont2.clock.reset)  # t=0 on next screen flip
                win.callOnFlip(instr_cont2.clearEvents, eventType='keyboard')  # clear events on next screen flip
            if instr_cont2.status == STARTED and not waitOnFlip:
                theseKeys = instr_cont2.getKeys(keyList=['space'], ignoreKeys=["escape"], waitRelease=False)
                _instr_cont2_allKeys.extend(theseKeys)
                if len(_instr_cont2_allKeys):
                    instr_cont2.keys = _instr_cont2_allKeys[-1].name  # just the last key pressed
                    instr_cont2.rt = _instr_cont2_allKeys[-1].rt
                    instr_cont2.duration = _instr_cont2_allKeys[-1].duration
                    # a response ends the routine
                    continueRoutine = False
            
            # check for quit (typically the Esc key)
            if defaultKeyboard.getKeys(keyList=["escape"]):
                thisExp.status = FINISHED
            if thisExp.status == FINISHED or endExpNow:
                endExperiment(thisExp, win=win)
                return
            # pause experiment here if requested
            if thisExp.status == PAUSED:
                pauseExperiment(
                    thisExp=thisExp, 
                    win=win, 
                    timers=[routineTimer], 
                    playbackComponents=[]
                )
                # skip the frame we paused on
                continue
            
            # check if all components have finished
            if not continueRoutine:  # a component has requested a forced-end of Routine
                instructions2.forceEnded = routineForceEnded = True
                break
            continueRoutine = False  # will revert to True if at least one component still running
            for thisComponent in instructions2.components:
                if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                    continueRoutine = True
                    break  # at least one component has not yet finished
            
            # refresh the screen
            if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
                win.flip()
        
        # --- Ending Routine "instructions2" ---
        for thisComponent in instructions2.components:
            if hasattr(thisComponent, "setAutoDraw"):
                thisComponent.setAutoDraw(False)
        # store stop times for instructions2
        instructions2.tStop = globalClock.getTime(format='float')
        instructions2.tStopRefresh = tThisFlipGlobal
        thisExp.addData('instructions2.stopped', instructions2.tStop)
        # check responses
        if instr_cont2.keys in ['', [], None]:  # No response was made
            instr_cont2.keys = None
        instructions_loop.addData('instr_cont2.keys',instr_cont2.keys)
        if instr_cont2.keys != None:  # we had a response
            instructions_loop.addData('instr_cont2.rt', instr_cont2.rt)
            instructions_loop.addData('instr_cont2.duration', instr_cont2.duration)
        # the Routine "instructions2" was not non-slip safe, so reset the non-slip timer
        routineTimer.reset()
        
        # --- Prepare to start Routine "instructions3" ---
        # create an object to store info about Routine instructions3
        instructions3 = data.Routine(
            name='instructions3',
            components=[instr3_1, instr3_2, instr3_3, instr3_4, instr_cont3, fixation_cross],
        )
        instructions3.status = NOT_STARTED
        continueRoutine = True
        # update component parameters for each repeat
        # create starting attributes for instr_cont3
        instr_cont3.keys = []
        instr_cont3.rt = []
        _instr_cont3_allKeys = []
        # store start times for instructions3
        instructions3.tStartRefresh = win.getFutureFlipTime(clock=globalClock)
        instructions3.tStart = globalClock.getTime(format='float')
        instructions3.status = STARTED
        thisExp.addData('instructions3.started', instructions3.tStart)
        instructions3.maxDuration = None
        # keep track of which components have finished
        instructions3Components = instructions3.components
        for thisComponent in instructions3.components:
            thisComponent.tStart = None
            thisComponent.tStop = None
            thisComponent.tStartRefresh = None
            thisComponent.tStopRefresh = None
            if hasattr(thisComponent, 'status'):
                thisComponent.status = NOT_STARTED
        # reset timers
        t = 0
        _timeToFirstFrame = win.getFutureFlipTime(clock="now")
        frameN = -1
        
        # --- Run Routine "instructions3" ---
        # if trial has changed, end Routine now
        if isinstance(instructions_loop, data.TrialHandler2) and thisInstructions_loop.thisN != instructions_loop.thisTrial.thisN:
            continueRoutine = False
        instructions3.forceEnded = routineForceEnded = not continueRoutine
        while continueRoutine:
            # get current time
            t = routineTimer.getTime()
            tThisFlip = win.getFutureFlipTime(clock=routineTimer)
            tThisFlipGlobal = win.getFutureFlipTime(clock=None)
            frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
            # update/draw components on each frame
            
            # *instr3_1* updates
            
            # if instr3_1 is starting this frame...
            if instr3_1.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                # keep track of start time/frame for later
                instr3_1.frameNStart = frameN  # exact frame index
                instr3_1.tStart = t  # local t and not account for scr refresh
                instr3_1.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(instr3_1, 'tStartRefresh')  # time at next scr refresh
                # add timestamp to datafile
                thisExp.timestampOnFlip(win, 'instr3_1.started')
                # update status
                instr3_1.status = STARTED
                instr3_1.setAutoDraw(True)
            
            # if instr3_1 is active this frame...
            if instr3_1.status == STARTED:
                # update params
                pass
            
            # *instr3_2* updates
            
            # if instr3_2 is starting this frame...
            if instr3_2.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                # keep track of start time/frame for later
                instr3_2.frameNStart = frameN  # exact frame index
                instr3_2.tStart = t  # local t and not account for scr refresh
                instr3_2.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(instr3_2, 'tStartRefresh')  # time at next scr refresh
                # add timestamp to datafile
                thisExp.timestampOnFlip(win, 'instr3_2.started')
                # update status
                instr3_2.status = STARTED
                instr3_2.setAutoDraw(True)
            
            # if instr3_2 is active this frame...
            if instr3_2.status == STARTED:
                # update params
                pass
            
            # *instr3_3* updates
            
            # if instr3_3 is starting this frame...
            if instr3_3.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                # keep track of start time/frame for later
                instr3_3.frameNStart = frameN  # exact frame index
                instr3_3.tStart = t  # local t and not account for scr refresh
                instr3_3.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(instr3_3, 'tStartRefresh')  # time at next scr refresh
                # add timestamp to datafile
                thisExp.timestampOnFlip(win, 'instr3_3.started')
                # update status
                instr3_3.status = STARTED
                instr3_3.setAutoDraw(True)
            
            # if instr3_3 is active this frame...
            if instr3_3.status == STARTED:
                # update params
                pass
            
            # *instr3_4* updates
            
            # if instr3_4 is starting this frame...
            if instr3_4.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                # keep track of start time/frame for later
                instr3_4.frameNStart = frameN  # exact frame index
                instr3_4.tStart = t  # local t and not account for scr refresh
                instr3_4.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(instr3_4, 'tStartRefresh')  # time at next scr refresh
                # add timestamp to datafile
                thisExp.timestampOnFlip(win, 'instr3_4.started')
                # update status
                instr3_4.status = STARTED
                instr3_4.setAutoDraw(True)
            
            # if instr3_4 is active this frame...
            if instr3_4.status == STARTED:
                # update params
                pass
            
            # *instr_cont3* updates
            waitOnFlip = False
            
            # if instr_cont3 is starting this frame...
            if instr_cont3.status == NOT_STARTED and tThisFlip >= 3.0-frameTolerance:
                # keep track of start time/frame for later
                instr_cont3.frameNStart = frameN  # exact frame index
                instr_cont3.tStart = t  # local t and not account for scr refresh
                instr_cont3.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(instr_cont3, 'tStartRefresh')  # time at next scr refresh
                # add timestamp to datafile
                thisExp.timestampOnFlip(win, 'instr_cont3.started')
                # update status
                instr_cont3.status = STARTED
                # keyboard checking is just starting
                waitOnFlip = True
                win.callOnFlip(instr_cont3.clock.reset)  # t=0 on next screen flip
                win.callOnFlip(instr_cont3.clearEvents, eventType='keyboard')  # clear events on next screen flip
            if instr_cont3.status == STARTED and not waitOnFlip:
                theseKeys = instr_cont3.getKeys(keyList=['space'], ignoreKeys=["escape"], waitRelease=False)
                _instr_cont3_allKeys.extend(theseKeys)
                if len(_instr_cont3_allKeys):
                    instr_cont3.keys = _instr_cont3_allKeys[-1].name  # just the last key pressed
                    instr_cont3.rt = _instr_cont3_allKeys[-1].rt
                    instr_cont3.duration = _instr_cont3_allKeys[-1].duration
                    # a response ends the routine
                    continueRoutine = False
            
            # *fixation_cross* updates
            
            # if fixation_cross is starting this frame...
            if fixation_cross.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                # keep track of start time/frame for later
                fixation_cross.frameNStart = frameN  # exact frame index
                fixation_cross.tStart = t  # local t and not account for scr refresh
                fixation_cross.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(fixation_cross, 'tStartRefresh')  # time at next scr refresh
                # add timestamp to datafile
                thisExp.timestampOnFlip(win, 'fixation_cross.started')
                # update status
                fixation_cross.status = STARTED
                fixation_cross.setAutoDraw(True)
            
            # if fixation_cross is active this frame...
            if fixation_cross.status == STARTED:
                # update params
                pass
            
            # if fixation_cross is stopping this frame...
            if fixation_cross.status == STARTED:
                # is it time to stop? (based on global clock, using actual start)
                if tThisFlipGlobal > fixation_cross.tStartRefresh + .5-frameTolerance:
                    # keep track of stop time/frame for later
                    fixation_cross.tStop = t  # not accounting for scr refresh
                    fixation_cross.tStopRefresh = tThisFlipGlobal  # on global time
                    fixation_cross.frameNStop = frameN  # exact frame index
                    # add timestamp to datafile
                    thisExp.timestampOnFlip(win, 'fixation_cross.stopped')
                    # update status
                    fixation_cross.status = FINISHED
                    fixation_cross.setAutoDraw(False)
            
            # check for quit (typically the Esc key)
            if defaultKeyboard.getKeys(keyList=["escape"]):
                thisExp.status = FINISHED
            if thisExp.status == FINISHED or endExpNow:
                endExperiment(thisExp, win=win)
                return
            # pause experiment here if requested
            if thisExp.status == PAUSED:
                pauseExperiment(
                    thisExp=thisExp, 
                    win=win, 
                    timers=[routineTimer], 
                    playbackComponents=[]
                )
                # skip the frame we paused on
                continue
            
            # check if all components have finished
            if not continueRoutine:  # a component has requested a forced-end of Routine
                instructions3.forceEnded = routineForceEnded = True
                break
            continueRoutine = False  # will revert to True if at least one component still running
            for thisComponent in instructions3.components:
                if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                    continueRoutine = True
                    break  # at least one component has not yet finished
            
            # refresh the screen
            if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
                win.flip()
        
        # --- Ending Routine "instructions3" ---
        for thisComponent in instructions3.components:
            if hasattr(thisComponent, "setAutoDraw"):
                thisComponent.setAutoDraw(False)
        # store stop times for instructions3
        instructions3.tStop = globalClock.getTime(format='float')
        instructions3.tStopRefresh = tThisFlipGlobal
        thisExp.addData('instructions3.stopped', instructions3.tStop)
        # check responses
        if instr_cont3.keys in ['', [], None]:  # No response was made
            instr_cont3.keys = None
        instructions_loop.addData('instr_cont3.keys',instr_cont3.keys)
        if instr_cont3.keys != None:  # we had a response
            instructions_loop.addData('instr_cont3.rt', instr_cont3.rt)
            instructions_loop.addData('instr_cont3.duration', instr_cont3.duration)
        # the Routine "instructions3" was not non-slip safe, so reset the non-slip timer
        routineTimer.reset()
        
        # --- Prepare to start Routine "instructions4" ---
        # create an object to store info about Routine instructions4
        instructions4 = data.Routine(
            name='instructions4',
            components=[instr4_1, instr4_2, instr4_3, instr4_4, instr_cont4],
        )
        instructions4.status = NOT_STARTED
        continueRoutine = True
        # update component parameters for each repeat
        # create starting attributes for instr_cont4
        instr_cont4.keys = []
        instr_cont4.rt = []
        _instr_cont4_allKeys = []
        # store start times for instructions4
        instructions4.tStartRefresh = win.getFutureFlipTime(clock=globalClock)
        instructions4.tStart = globalClock.getTime(format='float')
        instructions4.status = STARTED
        thisExp.addData('instructions4.started', instructions4.tStart)
        instructions4.maxDuration = None
        # keep track of which components have finished
        instructions4Components = instructions4.components
        for thisComponent in instructions4.components:
            thisComponent.tStart = None
            thisComponent.tStop = None
            thisComponent.tStartRefresh = None
            thisComponent.tStopRefresh = None
            if hasattr(thisComponent, 'status'):
                thisComponent.status = NOT_STARTED
        # reset timers
        t = 0
        _timeToFirstFrame = win.getFutureFlipTime(clock="now")
        frameN = -1
        
        # --- Run Routine "instructions4" ---
        # if trial has changed, end Routine now
        if isinstance(instructions_loop, data.TrialHandler2) and thisInstructions_loop.thisN != instructions_loop.thisTrial.thisN:
            continueRoutine = False
        instructions4.forceEnded = routineForceEnded = not continueRoutine
        while continueRoutine:
            # get current time
            t = routineTimer.getTime()
            tThisFlip = win.getFutureFlipTime(clock=routineTimer)
            tThisFlipGlobal = win.getFutureFlipTime(clock=None)
            frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
            # update/draw components on each frame
            
            # *instr4_1* updates
            
            # if instr4_1 is starting this frame...
            if instr4_1.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                # keep track of start time/frame for later
                instr4_1.frameNStart = frameN  # exact frame index
                instr4_1.tStart = t  # local t and not account for scr refresh
                instr4_1.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(instr4_1, 'tStartRefresh')  # time at next scr refresh
                # add timestamp to datafile
                thisExp.timestampOnFlip(win, 'instr4_1.started')
                # update status
                instr4_1.status = STARTED
                instr4_1.setAutoDraw(True)
            
            # if instr4_1 is active this frame...
            if instr4_1.status == STARTED:
                # update params
                pass
            
            # *instr4_2* updates
            
            # if instr4_2 is starting this frame...
            if instr4_2.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                # keep track of start time/frame for later
                instr4_2.frameNStart = frameN  # exact frame index
                instr4_2.tStart = t  # local t and not account for scr refresh
                instr4_2.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(instr4_2, 'tStartRefresh')  # time at next scr refresh
                # add timestamp to datafile
                thisExp.timestampOnFlip(win, 'instr4_2.started')
                # update status
                instr4_2.status = STARTED
                instr4_2.setAutoDraw(True)
            
            # if instr4_2 is active this frame...
            if instr4_2.status == STARTED:
                # update params
                pass
            
            # *instr4_3* updates
            
            # if instr4_3 is starting this frame...
            if instr4_3.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                # keep track of start time/frame for later
                instr4_3.frameNStart = frameN  # exact frame index
                instr4_3.tStart = t  # local t and not account for scr refresh
                instr4_3.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(instr4_3, 'tStartRefresh')  # time at next scr refresh
                # add timestamp to datafile
                thisExp.timestampOnFlip(win, 'instr4_3.started')
                # update status
                instr4_3.status = STARTED
                instr4_3.setAutoDraw(True)
            
            # if instr4_3 is active this frame...
            if instr4_3.status == STARTED:
                # update params
                pass
            
            # *instr4_4* updates
            
            # if instr4_4 is starting this frame...
            if instr4_4.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                # keep track of start time/frame for later
                instr4_4.frameNStart = frameN  # exact frame index
                instr4_4.tStart = t  # local t and not account for scr refresh
                instr4_4.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(instr4_4, 'tStartRefresh')  # time at next scr refresh
                # add timestamp to datafile
                thisExp.timestampOnFlip(win, 'instr4_4.started')
                # update status
                instr4_4.status = STARTED
                instr4_4.setAutoDraw(True)
            
            # if instr4_4 is active this frame...
            if instr4_4.status == STARTED:
                # update params
                pass
            
            # *instr_cont4* updates
            waitOnFlip = False
            
            # if instr_cont4 is starting this frame...
            if instr_cont4.status == NOT_STARTED and tThisFlip >= 3.0-frameTolerance:
                # keep track of start time/frame for later
                instr_cont4.frameNStart = frameN  # exact frame index
                instr_cont4.tStart = t  # local t and not account for scr refresh
                instr_cont4.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(instr_cont4, 'tStartRefresh')  # time at next scr refresh
                # add timestamp to datafile
                thisExp.timestampOnFlip(win, 'instr_cont4.started')
                # update status
                instr_cont4.status = STARTED
                # keyboard checking is just starting
                waitOnFlip = True
                win.callOnFlip(instr_cont4.clock.reset)  # t=0 on next screen flip
                win.callOnFlip(instr_cont4.clearEvents, eventType='keyboard')  # clear events on next screen flip
            if instr_cont4.status == STARTED and not waitOnFlip:
                theseKeys = instr_cont4.getKeys(keyList=['space'], ignoreKeys=["escape"], waitRelease=False)
                _instr_cont4_allKeys.extend(theseKeys)
                if len(_instr_cont4_allKeys):
                    instr_cont4.keys = _instr_cont4_allKeys[-1].name  # just the last key pressed
                    instr_cont4.rt = _instr_cont4_allKeys[-1].rt
                    instr_cont4.duration = _instr_cont4_allKeys[-1].duration
                    # a response ends the routine
                    continueRoutine = False
            
            # check for quit (typically the Esc key)
            if defaultKeyboard.getKeys(keyList=["escape"]):
                thisExp.status = FINISHED
            if thisExp.status == FINISHED or endExpNow:
                endExperiment(thisExp, win=win)
                return
            # pause experiment here if requested
            if thisExp.status == PAUSED:
                pauseExperiment(
                    thisExp=thisExp, 
                    win=win, 
                    timers=[routineTimer], 
                    playbackComponents=[]
                )
                # skip the frame we paused on
                continue
            
            # check if all components have finished
            if not continueRoutine:  # a component has requested a forced-end of Routine
                instructions4.forceEnded = routineForceEnded = True
                break
            continueRoutine = False  # will revert to True if at least one component still running
            for thisComponent in instructions4.components:
                if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                    continueRoutine = True
                    break  # at least one component has not yet finished
            
            # refresh the screen
            if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
                win.flip()
        
        # --- Ending Routine "instructions4" ---
        for thisComponent in instructions4.components:
            if hasattr(thisComponent, "setAutoDraw"):
                thisComponent.setAutoDraw(False)
        # store stop times for instructions4
        instructions4.tStop = globalClock.getTime(format='float')
        instructions4.tStopRefresh = tThisFlipGlobal
        thisExp.addData('instructions4.stopped', instructions4.tStop)
        # check responses
        if instr_cont4.keys in ['', [], None]:  # No response was made
            instr_cont4.keys = None
        instructions_loop.addData('instr_cont4.keys',instr_cont4.keys)
        if instr_cont4.keys != None:  # we had a response
            instructions_loop.addData('instr_cont4.rt', instr_cont4.rt)
            instructions_loop.addData('instr_cont4.duration', instr_cont4.duration)
        # the Routine "instructions4" was not non-slip safe, so reset the non-slip timer
        routineTimer.reset()
        
        # --- Prepare to start Routine "instructions5" ---
        # create an object to store info about Routine instructions5
        instructions5 = data.Routine(
            name='instructions5',
            components=[instr5_1, instr5_2, instr5_3, instr_cont5],
        )
        instructions5.status = NOT_STARTED
        continueRoutine = True
        # update component parameters for each repeat
        # create starting attributes for instr_cont5
        instr_cont5.keys = []
        instr_cont5.rt = []
        _instr_cont5_allKeys = []
        # store start times for instructions5
        instructions5.tStartRefresh = win.getFutureFlipTime(clock=globalClock)
        instructions5.tStart = globalClock.getTime(format='float')
        instructions5.status = STARTED
        thisExp.addData('instructions5.started', instructions5.tStart)
        instructions5.maxDuration = None
        # keep track of which components have finished
        instructions5Components = instructions5.components
        for thisComponent in instructions5.components:
            thisComponent.tStart = None
            thisComponent.tStop = None
            thisComponent.tStartRefresh = None
            thisComponent.tStopRefresh = None
            if hasattr(thisComponent, 'status'):
                thisComponent.status = NOT_STARTED
        # reset timers
        t = 0
        _timeToFirstFrame = win.getFutureFlipTime(clock="now")
        frameN = -1
        
        # --- Run Routine "instructions5" ---
        # if trial has changed, end Routine now
        if isinstance(instructions_loop, data.TrialHandler2) and thisInstructions_loop.thisN != instructions_loop.thisTrial.thisN:
            continueRoutine = False
        instructions5.forceEnded = routineForceEnded = not continueRoutine
        while continueRoutine:
            # get current time
            t = routineTimer.getTime()
            tThisFlip = win.getFutureFlipTime(clock=routineTimer)
            tThisFlipGlobal = win.getFutureFlipTime(clock=None)
            frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
            # update/draw components on each frame
            
            # *instr5_1* updates
            
            # if instr5_1 is starting this frame...
            if instr5_1.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                # keep track of start time/frame for later
                instr5_1.frameNStart = frameN  # exact frame index
                instr5_1.tStart = t  # local t and not account for scr refresh
                instr5_1.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(instr5_1, 'tStartRefresh')  # time at next scr refresh
                # add timestamp to datafile
                thisExp.timestampOnFlip(win, 'instr5_1.started')
                # update status
                instr5_1.status = STARTED
                instr5_1.setAutoDraw(True)
            
            # if instr5_1 is active this frame...
            if instr5_1.status == STARTED:
                # update params
                pass
            
            # *instr5_2* updates
            
            # if instr5_2 is starting this frame...
            if instr5_2.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                # keep track of start time/frame for later
                instr5_2.frameNStart = frameN  # exact frame index
                instr5_2.tStart = t  # local t and not account for scr refresh
                instr5_2.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(instr5_2, 'tStartRefresh')  # time at next scr refresh
                # add timestamp to datafile
                thisExp.timestampOnFlip(win, 'instr5_2.started')
                # update status
                instr5_2.status = STARTED
                instr5_2.setAutoDraw(True)
            
            # if instr5_2 is active this frame...
            if instr5_2.status == STARTED:
                # update params
                pass
            
            # *instr5_3* updates
            
            # if instr5_3 is starting this frame...
            if instr5_3.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                # keep track of start time/frame for later
                instr5_3.frameNStart = frameN  # exact frame index
                instr5_3.tStart = t  # local t and not account for scr refresh
                instr5_3.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(instr5_3, 'tStartRefresh')  # time at next scr refresh
                # add timestamp to datafile
                thisExp.timestampOnFlip(win, 'instr5_3.started')
                # update status
                instr5_3.status = STARTED
                instr5_3.setAutoDraw(True)
            
            # if instr5_3 is active this frame...
            if instr5_3.status == STARTED:
                # update params
                pass
            
            # *instr_cont5* updates
            waitOnFlip = False
            
            # if instr_cont5 is starting this frame...
            if instr_cont5.status == NOT_STARTED and tThisFlip >= 3.0-frameTolerance:
                # keep track of start time/frame for later
                instr_cont5.frameNStart = frameN  # exact frame index
                instr_cont5.tStart = t  # local t and not account for scr refresh
                instr_cont5.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(instr_cont5, 'tStartRefresh')  # time at next scr refresh
                # add timestamp to datafile
                thisExp.timestampOnFlip(win, 'instr_cont5.started')
                # update status
                instr_cont5.status = STARTED
                # keyboard checking is just starting
                waitOnFlip = True
                win.callOnFlip(instr_cont5.clock.reset)  # t=0 on next screen flip
                win.callOnFlip(instr_cont5.clearEvents, eventType='keyboard')  # clear events on next screen flip
            if instr_cont5.status == STARTED and not waitOnFlip:
                theseKeys = instr_cont5.getKeys(keyList=['space'], ignoreKeys=["escape"], waitRelease=False)
                _instr_cont5_allKeys.extend(theseKeys)
                if len(_instr_cont5_allKeys):
                    instr_cont5.keys = _instr_cont5_allKeys[-1].name  # just the last key pressed
                    instr_cont5.rt = _instr_cont5_allKeys[-1].rt
                    instr_cont5.duration = _instr_cont5_allKeys[-1].duration
                    # a response ends the routine
                    continueRoutine = False
            
            # check for quit (typically the Esc key)
            if defaultKeyboard.getKeys(keyList=["escape"]):
                thisExp.status = FINISHED
            if thisExp.status == FINISHED or endExpNow:
                endExperiment(thisExp, win=win)
                return
            # pause experiment here if requested
            if thisExp.status == PAUSED:
                pauseExperiment(
                    thisExp=thisExp, 
                    win=win, 
                    timers=[routineTimer], 
                    playbackComponents=[]
                )
                # skip the frame we paused on
                continue
            
            # check if all components have finished
            if not continueRoutine:  # a component has requested a forced-end of Routine
                instructions5.forceEnded = routineForceEnded = True
                break
            continueRoutine = False  # will revert to True if at least one component still running
            for thisComponent in instructions5.components:
                if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                    continueRoutine = True
                    break  # at least one component has not yet finished
            
            # refresh the screen
            if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
                win.flip()
        
        # --- Ending Routine "instructions5" ---
        for thisComponent in instructions5.components:
            if hasattr(thisComponent, "setAutoDraw"):
                thisComponent.setAutoDraw(False)
        # store stop times for instructions5
        instructions5.tStop = globalClock.getTime(format='float')
        instructions5.tStopRefresh = tThisFlipGlobal
        thisExp.addData('instructions5.stopped', instructions5.tStop)
        # check responses
        if instr_cont5.keys in ['', [], None]:  # No response was made
            instr_cont5.keys = None
        instructions_loop.addData('instr_cont5.keys',instr_cont5.keys)
        if instr_cont5.keys != None:  # we had a response
            instructions_loop.addData('instr_cont5.rt', instr_cont5.rt)
            instructions_loop.addData('instr_cont5.duration', instr_cont5.duration)
        # the Routine "instructions5" was not non-slip safe, so reset the non-slip timer
        routineTimer.reset()
        
        # --- Prepare to start Routine "instructions6" ---
        # create an object to store info about Routine instructions6
        instructions6 = data.Routine(
            name='instructions6',
            components=[instr6_1, instr6_2, instr6_3, instr_cont6],
        )
        instructions6.status = NOT_STARTED
        continueRoutine = True
        # update component parameters for each repeat
        # create starting attributes for instr_cont6
        instr_cont6.keys = []
        instr_cont6.rt = []
        _instr_cont6_allKeys = []
        # store start times for instructions6
        instructions6.tStartRefresh = win.getFutureFlipTime(clock=globalClock)
        instructions6.tStart = globalClock.getTime(format='float')
        instructions6.status = STARTED
        thisExp.addData('instructions6.started', instructions6.tStart)
        instructions6.maxDuration = None
        # keep track of which components have finished
        instructions6Components = instructions6.components
        for thisComponent in instructions6.components:
            thisComponent.tStart = None
            thisComponent.tStop = None
            thisComponent.tStartRefresh = None
            thisComponent.tStopRefresh = None
            if hasattr(thisComponent, 'status'):
                thisComponent.status = NOT_STARTED
        # reset timers
        t = 0
        _timeToFirstFrame = win.getFutureFlipTime(clock="now")
        frameN = -1
        
        # --- Run Routine "instructions6" ---
        # if trial has changed, end Routine now
        if isinstance(instructions_loop, data.TrialHandler2) and thisInstructions_loop.thisN != instructions_loop.thisTrial.thisN:
            continueRoutine = False
        instructions6.forceEnded = routineForceEnded = not continueRoutine
        while continueRoutine:
            # get current time
            t = routineTimer.getTime()
            tThisFlip = win.getFutureFlipTime(clock=routineTimer)
            tThisFlipGlobal = win.getFutureFlipTime(clock=None)
            frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
            # update/draw components on each frame
            
            # *instr6_1* updates
            
            # if instr6_1 is starting this frame...
            if instr6_1.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                # keep track of start time/frame for later
                instr6_1.frameNStart = frameN  # exact frame index
                instr6_1.tStart = t  # local t and not account for scr refresh
                instr6_1.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(instr6_1, 'tStartRefresh')  # time at next scr refresh
                # add timestamp to datafile
                thisExp.timestampOnFlip(win, 'instr6_1.started')
                # update status
                instr6_1.status = STARTED
                instr6_1.setAutoDraw(True)
            
            # if instr6_1 is active this frame...
            if instr6_1.status == STARTED:
                # update params
                pass
            
            # *instr6_2* updates
            
            # if instr6_2 is starting this frame...
            if instr6_2.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                # keep track of start time/frame for later
                instr6_2.frameNStart = frameN  # exact frame index
                instr6_2.tStart = t  # local t and not account for scr refresh
                instr6_2.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(instr6_2, 'tStartRefresh')  # time at next scr refresh
                # add timestamp to datafile
                thisExp.timestampOnFlip(win, 'instr6_2.started')
                # update status
                instr6_2.status = STARTED
                instr6_2.setAutoDraw(True)
            
            # if instr6_2 is active this frame...
            if instr6_2.status == STARTED:
                # update params
                pass
            
            # *instr6_3* updates
            
            # if instr6_3 is starting this frame...
            if instr6_3.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                # keep track of start time/frame for later
                instr6_3.frameNStart = frameN  # exact frame index
                instr6_3.tStart = t  # local t and not account for scr refresh
                instr6_3.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(instr6_3, 'tStartRefresh')  # time at next scr refresh
                # add timestamp to datafile
                thisExp.timestampOnFlip(win, 'instr6_3.started')
                # update status
                instr6_3.status = STARTED
                instr6_3.setAutoDraw(True)
            
            # if instr6_3 is active this frame...
            if instr6_3.status == STARTED:
                # update params
                pass
            
            # *instr_cont6* updates
            waitOnFlip = False
            
            # if instr_cont6 is starting this frame...
            if instr_cont6.status == NOT_STARTED and tThisFlip >= 3.0-frameTolerance:
                # keep track of start time/frame for later
                instr_cont6.frameNStart = frameN  # exact frame index
                instr_cont6.tStart = t  # local t and not account for scr refresh
                instr_cont6.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(instr_cont6, 'tStartRefresh')  # time at next scr refresh
                # add timestamp to datafile
                thisExp.timestampOnFlip(win, 'instr_cont6.started')
                # update status
                instr_cont6.status = STARTED
                # keyboard checking is just starting
                waitOnFlip = True
                win.callOnFlip(instr_cont6.clock.reset)  # t=0 on next screen flip
                win.callOnFlip(instr_cont6.clearEvents, eventType='keyboard')  # clear events on next screen flip
            if instr_cont6.status == STARTED and not waitOnFlip:
                theseKeys = instr_cont6.getKeys(keyList=['space'], ignoreKeys=["escape"], waitRelease=False)
                _instr_cont6_allKeys.extend(theseKeys)
                if len(_instr_cont6_allKeys):
                    instr_cont6.keys = _instr_cont6_allKeys[-1].name  # just the last key pressed
                    instr_cont6.rt = _instr_cont6_allKeys[-1].rt
                    instr_cont6.duration = _instr_cont6_allKeys[-1].duration
                    # a response ends the routine
                    continueRoutine = False
            
            # check for quit (typically the Esc key)
            if defaultKeyboard.getKeys(keyList=["escape"]):
                thisExp.status = FINISHED
            if thisExp.status == FINISHED or endExpNow:
                endExperiment(thisExp, win=win)
                return
            # pause experiment here if requested
            if thisExp.status == PAUSED:
                pauseExperiment(
                    thisExp=thisExp, 
                    win=win, 
                    timers=[routineTimer], 
                    playbackComponents=[]
                )
                # skip the frame we paused on
                continue
            
            # check if all components have finished
            if not continueRoutine:  # a component has requested a forced-end of Routine
                instructions6.forceEnded = routineForceEnded = True
                break
            continueRoutine = False  # will revert to True if at least one component still running
            for thisComponent in instructions6.components:
                if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                    continueRoutine = True
                    break  # at least one component has not yet finished
            
            # refresh the screen
            if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
                win.flip()
        
        # --- Ending Routine "instructions6" ---
        for thisComponent in instructions6.components:
            if hasattr(thisComponent, "setAutoDraw"):
                thisComponent.setAutoDraw(False)
        # store stop times for instructions6
        instructions6.tStop = globalClock.getTime(format='float')
        instructions6.tStopRefresh = tThisFlipGlobal
        thisExp.addData('instructions6.stopped', instructions6.tStop)
        # check responses
        if instr_cont6.keys in ['', [], None]:  # No response was made
            instr_cont6.keys = None
        instructions_loop.addData('instr_cont6.keys',instr_cont6.keys)
        if instr_cont6.keys != None:  # we had a response
            instructions_loop.addData('instr_cont6.rt', instr_cont6.rt)
            instructions_loop.addData('instr_cont6.duration', instr_cont6.duration)
        # the Routine "instructions6" was not non-slip safe, so reset the non-slip timer
        routineTimer.reset()
    # completed 0.0 repeats of 'instructions_loop'
    
    
    # --- Prepare to start Routine "read_in_demo_vals" ---
    # create an object to store info about Routine read_in_demo_vals
    read_in_demo_vals = data.Routine(
        name='read_in_demo_vals',
        components=[],
    )
    read_in_demo_vals.status = NOT_STARTED
    continueRoutine = True
    # update component parameters for each repeat
    # Run 'Begin Routine' code from read_demo_stim
    import numpy as np
    def read_demo(f):
        s=np.loadtxt(f)
        return s
    demo_in_vals = read_demo(demo_in_condition_file)
    demo_out_vals = read_demo(demo_out_condition_file)
    
    # store start times for read_in_demo_vals
    read_in_demo_vals.tStartRefresh = win.getFutureFlipTime(clock=globalClock)
    read_in_demo_vals.tStart = globalClock.getTime(format='float')
    read_in_demo_vals.status = STARTED
    thisExp.addData('read_in_demo_vals.started', read_in_demo_vals.tStart)
    read_in_demo_vals.maxDuration = None
    # keep track of which components have finished
    read_in_demo_valsComponents = read_in_demo_vals.components
    for thisComponent in read_in_demo_vals.components:
        thisComponent.tStart = None
        thisComponent.tStop = None
        thisComponent.tStartRefresh = None
        thisComponent.tStopRefresh = None
        if hasattr(thisComponent, 'status'):
            thisComponent.status = NOT_STARTED
    # reset timers
    t = 0
    _timeToFirstFrame = win.getFutureFlipTime(clock="now")
    frameN = -1
    
    # --- Run Routine "read_in_demo_vals" ---
    read_in_demo_vals.forceEnded = routineForceEnded = not continueRoutine
    while continueRoutine:
        # get current time
        t = routineTimer.getTime()
        tThisFlip = win.getFutureFlipTime(clock=routineTimer)
        tThisFlipGlobal = win.getFutureFlipTime(clock=None)
        frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
        # update/draw components on each frame
        
        # check for quit (typically the Esc key)
        if defaultKeyboard.getKeys(keyList=["escape"]):
            thisExp.status = FINISHED
        if thisExp.status == FINISHED or endExpNow:
            endExperiment(thisExp, win=win)
            return
        # pause experiment here if requested
        if thisExp.status == PAUSED:
            pauseExperiment(
                thisExp=thisExp, 
                win=win, 
                timers=[routineTimer], 
                playbackComponents=[]
            )
            # skip the frame we paused on
            continue
        
        # check if all components have finished
        if not continueRoutine:  # a component has requested a forced-end of Routine
            read_in_demo_vals.forceEnded = routineForceEnded = True
            break
        continueRoutine = False  # will revert to True if at least one component still running
        for thisComponent in read_in_demo_vals.components:
            if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                continueRoutine = True
                break  # at least one component has not yet finished
        
        # refresh the screen
        if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
            win.flip()
    
    # --- Ending Routine "read_in_demo_vals" ---
    for thisComponent in read_in_demo_vals.components:
        if hasattr(thisComponent, "setAutoDraw"):
            thisComponent.setAutoDraw(False)
    # store stop times for read_in_demo_vals
    read_in_demo_vals.tStop = globalClock.getTime(format='float')
    read_in_demo_vals.tStopRefresh = tThisFlipGlobal
    thisExp.addData('read_in_demo_vals.stopped', read_in_demo_vals.tStop)
    thisExp.nextEntry()
    # the Routine "read_in_demo_vals" was not non-slip safe, so reset the non-slip timer
    routineTimer.reset()
    
    # set up handler to look after randomisation of conditions etc
    demo_loop = data.TrialHandler2(
        name='demo_loop',
        nReps=0.0, 
        method='sequential', 
        extraInfo=expInfo, 
        originPath=-1, 
        trialList=[None], 
        seed=None, 
    )
    thisExp.addLoop(demo_loop)  # add the loop to the experiment
    thisDemo_loop = demo_loop.trialList[0]  # so we can initialise stimuli with some values
    # abbreviate parameter names if possible (e.g. rgb = thisDemo_loop.rgb)
    if thisDemo_loop != None:
        for paramName in thisDemo_loop:
            globals()[paramName] = thisDemo_loop[paramName]
    
    for thisDemo_loop in demo_loop:
        currentLoop = demo_loop
        thisExp.timestampOnFlip(win, 'thisRow.t', format=globalClock.format)
        # abbreviate parameter names if possible (e.g. rgb = thisDemo_loop.rgb)
        if thisDemo_loop != None:
            for paramName in thisDemo_loop:
                globals()[paramName] = thisDemo_loop[paramName]
        
        # --- Prepare to start Routine "learn_demo_in" ---
        # create an object to store info about Routine learn_demo_in
        learn_demo_in = data.Routine(
            name='learn_demo_in',
            components=[],
        )
        learn_demo_in.status = NOT_STARTED
        continueRoutine = True
        # update component parameters for each repeat
        # Run 'Begin Routine' code from learn_demo_code
        win.units='pix'
        y_locs=0
        n_demo=demo_in_vals.size
        demo_in_vals_shuffled = np.random.choice(np.arange(0,n_demo),n_demo,replace=False)
        demo_out_vals_shuffled = np.random.choice(np.arange(0,n_demo),n_demo,replace=False)
        total_range = (dist_btw_demo * n_demo)/2
        x_locs_current = -total_range
        
        line_list = []
        
        for i in demo_in_vals_shuffled:
            x_locs_current=x_locs_current+dist_btw_demo
            line_list.append(visual.Line(win,
                             #size=(demo_in_vals_shuffled[i],line_width),
                             #pos=(x_locs_current, y_locs+demo_in_vals_shuffled[i]/2),
                             start=(x_locs_current,y_locs),
                             end=(x_locs_current,y_locs+demo_in_vals[i]),
                             lineWidth=line_width,
                             lineColor=(0,0,0),
                             colorSpace='rgb',
                             fillColor=(-1,-1,-1)))
        for i in line_list:
            i.draw()    
        txt = visual.TextStim(win, 'Here are some lines that belong IN the Category.',color='black',
        pos=(0,-100))
        txt.draw()
            
        start_time = core.getTime()
        win.flip()
        core.wait(demo_dur)
        
        line_list = []
        total_range = (dist_btw_demo * n_demo)/2
        x_locs_current = -total_range
        
        for i in demo_out_vals_shuffled:
            x_locs_current=x_locs_current+dist_btw_demo
            line_list.append(visual.Line(win,
                             #size=(demo_in_vals_shuffled[i],line_width),
                             #pos=(x_locs_current, y_locs+demo_in_vals_shuffled[i]/2),
                             start=(x_locs_current,y_locs),
                             end=(x_locs_current,y_locs+demo_out_vals[i]),
                             lineWidth=line_width,
                             lineColor=(0,0,0),
                             colorSpace='rgb',
                             fillColor=(-1,-1,-1)))
        for i in line_list:
            i.draw()    
        txt = visual.TextStim(win, 'Here are some lines that DO NOT belong in the Category.',
        color='black',
        pos=(0,-100))
        txt.draw()
            
        start_time = core.getTime()
        win.flip()
        core.wait(demo_dur)
        
        # store start times for learn_demo_in
        learn_demo_in.tStartRefresh = win.getFutureFlipTime(clock=globalClock)
        learn_demo_in.tStart = globalClock.getTime(format='float')
        learn_demo_in.status = STARTED
        thisExp.addData('learn_demo_in.started', learn_demo_in.tStart)
        learn_demo_in.maxDuration = None
        # keep track of which components have finished
        learn_demo_inComponents = learn_demo_in.components
        for thisComponent in learn_demo_in.components:
            thisComponent.tStart = None
            thisComponent.tStop = None
            thisComponent.tStartRefresh = None
            thisComponent.tStopRefresh = None
            if hasattr(thisComponent, 'status'):
                thisComponent.status = NOT_STARTED
        # reset timers
        t = 0
        _timeToFirstFrame = win.getFutureFlipTime(clock="now")
        frameN = -1
        
        # --- Run Routine "learn_demo_in" ---
        # if trial has changed, end Routine now
        if isinstance(demo_loop, data.TrialHandler2) and thisDemo_loop.thisN != demo_loop.thisTrial.thisN:
            continueRoutine = False
        learn_demo_in.forceEnded = routineForceEnded = not continueRoutine
        while continueRoutine:
            # get current time
            t = routineTimer.getTime()
            tThisFlip = win.getFutureFlipTime(clock=routineTimer)
            tThisFlipGlobal = win.getFutureFlipTime(clock=None)
            frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
            # update/draw components on each frame
            
            # check for quit (typically the Esc key)
            if defaultKeyboard.getKeys(keyList=["escape"]):
                thisExp.status = FINISHED
            if thisExp.status == FINISHED or endExpNow:
                endExperiment(thisExp, win=win)
                return
            # pause experiment here if requested
            if thisExp.status == PAUSED:
                pauseExperiment(
                    thisExp=thisExp, 
                    win=win, 
                    timers=[routineTimer], 
                    playbackComponents=[]
                )
                # skip the frame we paused on
                continue
            
            # check if all components have finished
            if not continueRoutine:  # a component has requested a forced-end of Routine
                learn_demo_in.forceEnded = routineForceEnded = True
                break
            continueRoutine = False  # will revert to True if at least one component still running
            for thisComponent in learn_demo_in.components:
                if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                    continueRoutine = True
                    break  # at least one component has not yet finished
            
            # refresh the screen
            if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
                win.flip()
        
        # --- Ending Routine "learn_demo_in" ---
        for thisComponent in learn_demo_in.components:
            if hasattr(thisComponent, "setAutoDraw"):
                thisComponent.setAutoDraw(False)
        # store stop times for learn_demo_in
        learn_demo_in.tStop = globalClock.getTime(format='float')
        learn_demo_in.tStopRefresh = tThisFlipGlobal
        thisExp.addData('learn_demo_in.stopped', learn_demo_in.tStop)
        # the Routine "learn_demo_in" was not non-slip safe, so reset the non-slip timer
        routineTimer.reset()
        
        # --- Prepare to start Routine "learn_start" ---
        # create an object to store info about Routine learn_start
        learn_start = data.Routine(
            name='learn_start',
            components=[learn_start_text, learn_start_reps],
        )
        learn_start.status = NOT_STARTED
        continueRoutine = True
        # update component parameters for each repeat
        # create starting attributes for learn_start_reps
        learn_start_reps.keys = []
        learn_start_reps.rt = []
        _learn_start_reps_allKeys = []
        # store start times for learn_start
        learn_start.tStartRefresh = win.getFutureFlipTime(clock=globalClock)
        learn_start.tStart = globalClock.getTime(format='float')
        learn_start.status = STARTED
        thisExp.addData('learn_start.started', learn_start.tStart)
        learn_start.maxDuration = None
        # keep track of which components have finished
        learn_startComponents = learn_start.components
        for thisComponent in learn_start.components:
            thisComponent.tStart = None
            thisComponent.tStop = None
            thisComponent.tStartRefresh = None
            thisComponent.tStopRefresh = None
            if hasattr(thisComponent, 'status'):
                thisComponent.status = NOT_STARTED
        # reset timers
        t = 0
        _timeToFirstFrame = win.getFutureFlipTime(clock="now")
        frameN = -1
        
        # --- Run Routine "learn_start" ---
        # if trial has changed, end Routine now
        if isinstance(demo_loop, data.TrialHandler2) and thisDemo_loop.thisN != demo_loop.thisTrial.thisN:
            continueRoutine = False
        learn_start.forceEnded = routineForceEnded = not continueRoutine
        while continueRoutine:
            # get current time
            t = routineTimer.getTime()
            tThisFlip = win.getFutureFlipTime(clock=routineTimer)
            tThisFlipGlobal = win.getFutureFlipTime(clock=None)
            frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
            # update/draw components on each frame
            
            # *learn_start_text* updates
            
            # if learn_start_text is starting this frame...
            if learn_start_text.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                # keep track of start time/frame for later
                learn_start_text.frameNStart = frameN  # exact frame index
                learn_start_text.tStart = t  # local t and not account for scr refresh
                learn_start_text.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(learn_start_text, 'tStartRefresh')  # time at next scr refresh
                # add timestamp to datafile
                thisExp.timestampOnFlip(win, 'learn_start_text.started')
                # update status
                learn_start_text.status = STARTED
                learn_start_text.setAutoDraw(True)
            
            # if learn_start_text is active this frame...
            if learn_start_text.status == STARTED:
                # update params
                pass
            
            # *learn_start_reps* updates
            waitOnFlip = False
            
            # if learn_start_reps is starting this frame...
            if learn_start_reps.status == NOT_STARTED and tThisFlip >= .5-frameTolerance:
                # keep track of start time/frame for later
                learn_start_reps.frameNStart = frameN  # exact frame index
                learn_start_reps.tStart = t  # local t and not account for scr refresh
                learn_start_reps.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(learn_start_reps, 'tStartRefresh')  # time at next scr refresh
                # add timestamp to datafile
                thisExp.timestampOnFlip(win, 'learn_start_reps.started')
                # update status
                learn_start_reps.status = STARTED
                # keyboard checking is just starting
                waitOnFlip = True
                win.callOnFlip(learn_start_reps.clock.reset)  # t=0 on next screen flip
                win.callOnFlip(learn_start_reps.clearEvents, eventType='keyboard')  # clear events on next screen flip
            if learn_start_reps.status == STARTED and not waitOnFlip:
                theseKeys = learn_start_reps.getKeys(keyList=['space'], ignoreKeys=["escape"], waitRelease=False)
                _learn_start_reps_allKeys.extend(theseKeys)
                if len(_learn_start_reps_allKeys):
                    learn_start_reps.keys = _learn_start_reps_allKeys[-1].name  # just the last key pressed
                    learn_start_reps.rt = _learn_start_reps_allKeys[-1].rt
                    learn_start_reps.duration = _learn_start_reps_allKeys[-1].duration
                    # a response ends the routine
                    continueRoutine = False
            
            # check for quit (typically the Esc key)
            if defaultKeyboard.getKeys(keyList=["escape"]):
                thisExp.status = FINISHED
            if thisExp.status == FINISHED or endExpNow:
                endExperiment(thisExp, win=win)
                return
            # pause experiment here if requested
            if thisExp.status == PAUSED:
                pauseExperiment(
                    thisExp=thisExp, 
                    win=win, 
                    timers=[routineTimer], 
                    playbackComponents=[]
                )
                # skip the frame we paused on
                continue
            
            # check if all components have finished
            if not continueRoutine:  # a component has requested a forced-end of Routine
                learn_start.forceEnded = routineForceEnded = True
                break
            continueRoutine = False  # will revert to True if at least one component still running
            for thisComponent in learn_start.components:
                if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                    continueRoutine = True
                    break  # at least one component has not yet finished
            
            # refresh the screen
            if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
                win.flip()
        
        # --- Ending Routine "learn_start" ---
        for thisComponent in learn_start.components:
            if hasattr(thisComponent, "setAutoDraw"):
                thisComponent.setAutoDraw(False)
        # store stop times for learn_start
        learn_start.tStop = globalClock.getTime(format='float')
        learn_start.tStopRefresh = tThisFlipGlobal
        thisExp.addData('learn_start.stopped', learn_start.tStop)
        # check responses
        if learn_start_reps.keys in ['', [], None]:  # No response was made
            learn_start_reps.keys = None
        demo_loop.addData('learn_start_reps.keys',learn_start_reps.keys)
        if learn_start_reps.keys != None:  # we had a response
            demo_loop.addData('learn_start_reps.rt', learn_start_reps.rt)
            demo_loop.addData('learn_start_reps.duration', learn_start_reps.duration)
        # the Routine "learn_start" was not non-slip safe, so reset the non-slip timer
        routineTimer.reset()
    # completed 0.0 repeats of 'demo_loop'
    
    
    # set up handler to look after randomisation of conditions etc
    learn_trials = data.TrialHandler2(
        name='learn_trials',
        nReps=0.0, 
        method='random', 
        extraInfo=expInfo, 
        originPath=-1, 
        trialList=data.importConditions(learn_condition_file), 
        seed=None, 
    )
    thisExp.addLoop(learn_trials)  # add the loop to the experiment
    thisLearn_trial = learn_trials.trialList[0]  # so we can initialise stimuli with some values
    # abbreviate parameter names if possible (e.g. rgb = thisLearn_trial.rgb)
    if thisLearn_trial != None:
        for paramName in thisLearn_trial:
            globals()[paramName] = thisLearn_trial[paramName]
    if thisSession is not None:
        # if running in a Session with a Liaison client, send data up to now
        thisSession.sendExperimentData()
    
    for thisLearn_trial in learn_trials:
        currentLoop = learn_trials
        thisExp.timestampOnFlip(win, 'thisRow.t', format=globalClock.format)
        if thisSession is not None:
            # if running in a Session with a Liaison client, send data up to now
            thisSession.sendExperimentData()
        # abbreviate parameter names if possible (e.g. rgb = thisLearn_trial.rgb)
        if thisLearn_trial != None:
            for paramName in thisLearn_trial:
                globals()[paramName] = thisLearn_trial[paramName]
        
        # --- Prepare to start Routine "learn_trial" ---
        # create an object to store info about Routine learn_trial
        learn_trial = data.Routine(
            name='learn_trial',
            components=[line_learn, learn_prompt, key_resp],
        )
        learn_trial.status = NOT_STARTED
        continueRoutine = True
        # update component parameters for each repeat
        line_learn.setPos((0, 0))
        line_learn.setSize([line_width, line_len])
        # create starting attributes for key_resp
        key_resp.keys = []
        key_resp.rt = []
        _key_resp_allKeys = []
        # store start times for learn_trial
        learn_trial.tStartRefresh = win.getFutureFlipTime(clock=globalClock)
        learn_trial.tStart = globalClock.getTime(format='float')
        learn_trial.status = STARTED
        thisExp.addData('learn_trial.started', learn_trial.tStart)
        learn_trial.maxDuration = None
        # keep track of which components have finished
        learn_trialComponents = learn_trial.components
        for thisComponent in learn_trial.components:
            thisComponent.tStart = None
            thisComponent.tStop = None
            thisComponent.tStartRefresh = None
            thisComponent.tStopRefresh = None
            if hasattr(thisComponent, 'status'):
                thisComponent.status = NOT_STARTED
        # reset timers
        t = 0
        _timeToFirstFrame = win.getFutureFlipTime(clock="now")
        frameN = -1
        
        # --- Run Routine "learn_trial" ---
        # if trial has changed, end Routine now
        if isinstance(learn_trials, data.TrialHandler2) and thisLearn_trial.thisN != learn_trials.thisTrial.thisN:
            continueRoutine = False
        learn_trial.forceEnded = routineForceEnded = not continueRoutine
        while continueRoutine:
            # get current time
            t = routineTimer.getTime()
            tThisFlip = win.getFutureFlipTime(clock=routineTimer)
            tThisFlipGlobal = win.getFutureFlipTime(clock=None)
            frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
            # update/draw components on each frame
            
            # *line_learn* updates
            
            # if line_learn is starting this frame...
            if line_learn.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                # keep track of start time/frame for later
                line_learn.frameNStart = frameN  # exact frame index
                line_learn.tStart = t  # local t and not account for scr refresh
                line_learn.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(line_learn, 'tStartRefresh')  # time at next scr refresh
                # add timestamp to datafile
                thisExp.timestampOnFlip(win, 'line_learn.started')
                # update status
                line_learn.status = STARTED
                line_learn.setAutoDraw(True)
            
            # if line_learn is active this frame...
            if line_learn.status == STARTED:
                # update params
                pass
            
            # *learn_prompt* updates
            
            # if learn_prompt is starting this frame...
            if learn_prompt.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                # keep track of start time/frame for later
                learn_prompt.frameNStart = frameN  # exact frame index
                learn_prompt.tStart = t  # local t and not account for scr refresh
                learn_prompt.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(learn_prompt, 'tStartRefresh')  # time at next scr refresh
                # add timestamp to datafile
                thisExp.timestampOnFlip(win, 'learn_prompt.started')
                # update status
                learn_prompt.status = STARTED
                learn_prompt.setAutoDraw(True)
            
            # if learn_prompt is active this frame...
            if learn_prompt.status == STARTED:
                # update params
                pass
            
            # *key_resp* updates
            waitOnFlip = False
            
            # if key_resp is starting this frame...
            if key_resp.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                # keep track of start time/frame for later
                key_resp.frameNStart = frameN  # exact frame index
                key_resp.tStart = t  # local t and not account for scr refresh
                key_resp.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(key_resp, 'tStartRefresh')  # time at next scr refresh
                # add timestamp to datafile
                thisExp.timestampOnFlip(win, 'key_resp.started')
                # update status
                key_resp.status = STARTED
                # keyboard checking is just starting
                waitOnFlip = True
                win.callOnFlip(key_resp.clock.reset)  # t=0 on next screen flip
                win.callOnFlip(key_resp.clearEvents, eventType='keyboard')  # clear events on next screen flip
            if key_resp.status == STARTED and not waitOnFlip:
                theseKeys = key_resp.getKeys(keyList=['u','i'], ignoreKeys=["escape"], waitRelease=False)
                _key_resp_allKeys.extend(theseKeys)
                if len(_key_resp_allKeys):
                    key_resp.keys = _key_resp_allKeys[-1].name  # just the last key pressed
                    key_resp.rt = _key_resp_allKeys[-1].rt
                    key_resp.duration = _key_resp_allKeys[-1].duration
                    # was this correct?
                    if (key_resp.keys == str(key_correct)) or (key_resp.keys == key_correct):
                        key_resp.corr = 1
                    else:
                        key_resp.corr = 0
                    # a response ends the routine
                    continueRoutine = False
            
            # check for quit (typically the Esc key)
            if defaultKeyboard.getKeys(keyList=["escape"]):
                thisExp.status = FINISHED
            if thisExp.status == FINISHED or endExpNow:
                endExperiment(thisExp, win=win)
                return
            # pause experiment here if requested
            if thisExp.status == PAUSED:
                pauseExperiment(
                    thisExp=thisExp, 
                    win=win, 
                    timers=[routineTimer], 
                    playbackComponents=[]
                )
                # skip the frame we paused on
                continue
            
            # check if all components have finished
            if not continueRoutine:  # a component has requested a forced-end of Routine
                learn_trial.forceEnded = routineForceEnded = True
                break
            continueRoutine = False  # will revert to True if at least one component still running
            for thisComponent in learn_trial.components:
                if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                    continueRoutine = True
                    break  # at least one component has not yet finished
            
            # refresh the screen
            if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
                win.flip()
        
        # --- Ending Routine "learn_trial" ---
        for thisComponent in learn_trial.components:
            if hasattr(thisComponent, "setAutoDraw"):
                thisComponent.setAutoDraw(False)
        # store stop times for learn_trial
        learn_trial.tStop = globalClock.getTime(format='float')
        learn_trial.tStopRefresh = tThisFlipGlobal
        thisExp.addData('learn_trial.stopped', learn_trial.tStop)
        # check responses
        if key_resp.keys in ['', [], None]:  # No response was made
            key_resp.keys = None
            # was no response the correct answer?!
            if str(key_correct).lower() == 'none':
               key_resp.corr = 1;  # correct non-response
            else:
               key_resp.corr = 0;  # failed to respond (incorrectly)
        # store data for learn_trials (TrialHandler)
        learn_trials.addData('key_resp.keys',key_resp.keys)
        learn_trials.addData('key_resp.corr', key_resp.corr)
        if key_resp.keys != None:  # we had a response
            learn_trials.addData('key_resp.rt', key_resp.rt)
            learn_trials.addData('key_resp.duration', key_resp.duration)
        # the Routine "learn_trial" was not non-slip safe, so reset the non-slip timer
        routineTimer.reset()
        
        # --- Prepare to start Routine "learn_feedback" ---
        # create an object to store info about Routine learn_feedback
        learn_feedback = data.Routine(
            name='learn_feedback',
            components=[learn_feedback_text_display, line_for_feedback],
        )
        learn_feedback.status = NOT_STARTED
        continueRoutine = True
        # update component parameters for each repeat
        # Run 'Begin Routine' code from learn_feedback_text_code
        if key_resp.corr==1:
            learn_feedback_dur=1;
            learn_feedback_col="#008000"
            if distribution=="in":
                learn_feedback_text="You were correct! This line DOES belong in the category."
            elif distribution=="out":
                learn_feedback_text="You were correct! This line does NOT belong in the category."
        elif key_resp.corr==0:
            learn_feedback_dur=3;
            learn_feedback_col="#FF0000"
            if distribution=="in":
                learn_feedback_text="You were incorrect. This line DOES belong in the category."
            elif distribution=="out":
                learn_feedback_text="You were incorrect. This line does NOT belong in the category."
                    
                
        learn_feedback_text_display.setColor(learn_feedback_col, colorSpace='rgb')
        learn_feedback_text_display.setText(learn_feedback_text)
        line_for_feedback.setPos((0, 0))
        line_for_feedback.setSize([line_width, line_len])
        # store start times for learn_feedback
        learn_feedback.tStartRefresh = win.getFutureFlipTime(clock=globalClock)
        learn_feedback.tStart = globalClock.getTime(format='float')
        learn_feedback.status = STARTED
        thisExp.addData('learn_feedback.started', learn_feedback.tStart)
        learn_feedback.maxDuration = None
        # keep track of which components have finished
        learn_feedbackComponents = learn_feedback.components
        for thisComponent in learn_feedback.components:
            thisComponent.tStart = None
            thisComponent.tStop = None
            thisComponent.tStartRefresh = None
            thisComponent.tStopRefresh = None
            if hasattr(thisComponent, 'status'):
                thisComponent.status = NOT_STARTED
        # reset timers
        t = 0
        _timeToFirstFrame = win.getFutureFlipTime(clock="now")
        frameN = -1
        
        # --- Run Routine "learn_feedback" ---
        # if trial has changed, end Routine now
        if isinstance(learn_trials, data.TrialHandler2) and thisLearn_trial.thisN != learn_trials.thisTrial.thisN:
            continueRoutine = False
        learn_feedback.forceEnded = routineForceEnded = not continueRoutine
        while continueRoutine:
            # get current time
            t = routineTimer.getTime()
            tThisFlip = win.getFutureFlipTime(clock=routineTimer)
            tThisFlipGlobal = win.getFutureFlipTime(clock=None)
            frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
            # update/draw components on each frame
            
            # *learn_feedback_text_display* updates
            
            # if learn_feedback_text_display is starting this frame...
            if learn_feedback_text_display.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                # keep track of start time/frame for later
                learn_feedback_text_display.frameNStart = frameN  # exact frame index
                learn_feedback_text_display.tStart = t  # local t and not account for scr refresh
                learn_feedback_text_display.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(learn_feedback_text_display, 'tStartRefresh')  # time at next scr refresh
                # add timestamp to datafile
                thisExp.timestampOnFlip(win, 'learn_feedback_text_display.started')
                # update status
                learn_feedback_text_display.status = STARTED
                learn_feedback_text_display.setAutoDraw(True)
            
            # if learn_feedback_text_display is active this frame...
            if learn_feedback_text_display.status == STARTED:
                # update params
                pass
            
            # if learn_feedback_text_display is stopping this frame...
            if learn_feedback_text_display.status == STARTED:
                # is it time to stop? (based on global clock, using actual start)
                if tThisFlipGlobal > learn_feedback_text_display.tStartRefresh + learn_feedback_dur-frameTolerance:
                    # keep track of stop time/frame for later
                    learn_feedback_text_display.tStop = t  # not accounting for scr refresh
                    learn_feedback_text_display.tStopRefresh = tThisFlipGlobal  # on global time
                    learn_feedback_text_display.frameNStop = frameN  # exact frame index
                    # add timestamp to datafile
                    thisExp.timestampOnFlip(win, 'learn_feedback_text_display.stopped')
                    # update status
                    learn_feedback_text_display.status = FINISHED
                    learn_feedback_text_display.setAutoDraw(False)
            
            # *line_for_feedback* updates
            
            # if line_for_feedback is starting this frame...
            if line_for_feedback.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                # keep track of start time/frame for later
                line_for_feedback.frameNStart = frameN  # exact frame index
                line_for_feedback.tStart = t  # local t and not account for scr refresh
                line_for_feedback.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(line_for_feedback, 'tStartRefresh')  # time at next scr refresh
                # add timestamp to datafile
                thisExp.timestampOnFlip(win, 'line_for_feedback.started')
                # update status
                line_for_feedback.status = STARTED
                line_for_feedback.setAutoDraw(True)
            
            # if line_for_feedback is active this frame...
            if line_for_feedback.status == STARTED:
                # update params
                pass
            
            # if line_for_feedback is stopping this frame...
            if line_for_feedback.status == STARTED:
                # is it time to stop? (based on global clock, using actual start)
                if tThisFlipGlobal > line_for_feedback.tStartRefresh + learn_feedback_dur-frameTolerance:
                    # keep track of stop time/frame for later
                    line_for_feedback.tStop = t  # not accounting for scr refresh
                    line_for_feedback.tStopRefresh = tThisFlipGlobal  # on global time
                    line_for_feedback.frameNStop = frameN  # exact frame index
                    # add timestamp to datafile
                    thisExp.timestampOnFlip(win, 'line_for_feedback.stopped')
                    # update status
                    line_for_feedback.status = FINISHED
                    line_for_feedback.setAutoDraw(False)
            
            # check for quit (typically the Esc key)
            if defaultKeyboard.getKeys(keyList=["escape"]):
                thisExp.status = FINISHED
            if thisExp.status == FINISHED or endExpNow:
                endExperiment(thisExp, win=win)
                return
            # pause experiment here if requested
            if thisExp.status == PAUSED:
                pauseExperiment(
                    thisExp=thisExp, 
                    win=win, 
                    timers=[routineTimer], 
                    playbackComponents=[]
                )
                # skip the frame we paused on
                continue
            
            # check if all components have finished
            if not continueRoutine:  # a component has requested a forced-end of Routine
                learn_feedback.forceEnded = routineForceEnded = True
                break
            continueRoutine = False  # will revert to True if at least one component still running
            for thisComponent in learn_feedback.components:
                if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                    continueRoutine = True
                    break  # at least one component has not yet finished
            
            # refresh the screen
            if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
                win.flip()
        
        # --- Ending Routine "learn_feedback" ---
        for thisComponent in learn_feedback.components:
            if hasattr(thisComponent, "setAutoDraw"):
                thisComponent.setAutoDraw(False)
        # store stop times for learn_feedback
        learn_feedback.tStop = globalClock.getTime(format='float')
        learn_feedback.tStopRefresh = tThisFlipGlobal
        thisExp.addData('learn_feedback.stopped', learn_feedback.tStop)
        # the Routine "learn_feedback" was not non-slip safe, so reset the non-slip timer
        routineTimer.reset()
        
        # --- Prepare to start Routine "blank" ---
        # create an object to store info about Routine blank
        blank = data.Routine(
            name='blank',
            components=[blank_200],
        )
        blank.status = NOT_STARTED
        continueRoutine = True
        # update component parameters for each repeat
        # store start times for blank
        blank.tStartRefresh = win.getFutureFlipTime(clock=globalClock)
        blank.tStart = globalClock.getTime(format='float')
        blank.status = STARTED
        thisExp.addData('blank.started', blank.tStart)
        blank.maxDuration = None
        # keep track of which components have finished
        blankComponents = blank.components
        for thisComponent in blank.components:
            thisComponent.tStart = None
            thisComponent.tStop = None
            thisComponent.tStartRefresh = None
            thisComponent.tStopRefresh = None
            if hasattr(thisComponent, 'status'):
                thisComponent.status = NOT_STARTED
        # reset timers
        t = 0
        _timeToFirstFrame = win.getFutureFlipTime(clock="now")
        frameN = -1
        
        # --- Run Routine "blank" ---
        # if trial has changed, end Routine now
        if isinstance(learn_trials, data.TrialHandler2) and thisLearn_trial.thisN != learn_trials.thisTrial.thisN:
            continueRoutine = False
        blank.forceEnded = routineForceEnded = not continueRoutine
        while continueRoutine and routineTimer.getTime() < 0.2:
            # get current time
            t = routineTimer.getTime()
            tThisFlip = win.getFutureFlipTime(clock=routineTimer)
            tThisFlipGlobal = win.getFutureFlipTime(clock=None)
            frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
            # update/draw components on each frame
            
            # *blank_200* updates
            
            # if blank_200 is starting this frame...
            if blank_200.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                # keep track of start time/frame for later
                blank_200.frameNStart = frameN  # exact frame index
                blank_200.tStart = t  # local t and not account for scr refresh
                blank_200.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(blank_200, 'tStartRefresh')  # time at next scr refresh
                # add timestamp to datafile
                thisExp.timestampOnFlip(win, 'blank_200.started')
                # update status
                blank_200.status = STARTED
                blank_200.setAutoDraw(True)
            
            # if blank_200 is active this frame...
            if blank_200.status == STARTED:
                # update params
                pass
            
            # if blank_200 is stopping this frame...
            if blank_200.status == STARTED:
                # is it time to stop? (based on global clock, using actual start)
                if tThisFlipGlobal > blank_200.tStartRefresh + 0.2-frameTolerance:
                    # keep track of stop time/frame for later
                    blank_200.tStop = t  # not accounting for scr refresh
                    blank_200.tStopRefresh = tThisFlipGlobal  # on global time
                    blank_200.frameNStop = frameN  # exact frame index
                    # add timestamp to datafile
                    thisExp.timestampOnFlip(win, 'blank_200.stopped')
                    # update status
                    blank_200.status = FINISHED
                    blank_200.setAutoDraw(False)
            
            # check for quit (typically the Esc key)
            if defaultKeyboard.getKeys(keyList=["escape"]):
                thisExp.status = FINISHED
            if thisExp.status == FINISHED or endExpNow:
                endExperiment(thisExp, win=win)
                return
            # pause experiment here if requested
            if thisExp.status == PAUSED:
                pauseExperiment(
                    thisExp=thisExp, 
                    win=win, 
                    timers=[routineTimer], 
                    playbackComponents=[]
                )
                # skip the frame we paused on
                continue
            
            # check if all components have finished
            if not continueRoutine:  # a component has requested a forced-end of Routine
                blank.forceEnded = routineForceEnded = True
                break
            continueRoutine = False  # will revert to True if at least one component still running
            for thisComponent in blank.components:
                if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                    continueRoutine = True
                    break  # at least one component has not yet finished
            
            # refresh the screen
            if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
                win.flip()
        
        # --- Ending Routine "blank" ---
        for thisComponent in blank.components:
            if hasattr(thisComponent, "setAutoDraw"):
                thisComponent.setAutoDraw(False)
        # store stop times for blank
        blank.tStop = globalClock.getTime(format='float')
        blank.tStopRefresh = tThisFlipGlobal
        thisExp.addData('blank.stopped', blank.tStop)
        # using non-slip timing so subtract the expected duration of this Routine (unless ended on request)
        if blank.maxDurationReached:
            routineTimer.addTime(-blank.maxDuration)
        elif blank.forceEnded:
            routineTimer.reset()
        else:
            routineTimer.addTime(-0.200000)
        thisExp.nextEntry()
        
    # completed 0.0 repeats of 'learn_trials'
    
    if thisSession is not None:
        # if running in a Session with a Liaison client, send data up to now
        thisSession.sendExperimentData()
    
    # set up handler to look after randomisation of conditions etc
    transfer_instructions_loop = data.TrialHandler2(
        name='transfer_instructions_loop',
        nReps=0.0, 
        method='sequential', 
        extraInfo=expInfo, 
        originPath=-1, 
        trialList=[None], 
        seed=None, 
    )
    thisExp.addLoop(transfer_instructions_loop)  # add the loop to the experiment
    thisTransfer_instructions_loop = transfer_instructions_loop.trialList[0]  # so we can initialise stimuli with some values
    # abbreviate parameter names if possible (e.g. rgb = thisTransfer_instructions_loop.rgb)
    if thisTransfer_instructions_loop != None:
        for paramName in thisTransfer_instructions_loop:
            globals()[paramName] = thisTransfer_instructions_loop[paramName]
    
    for thisTransfer_instructions_loop in transfer_instructions_loop:
        currentLoop = transfer_instructions_loop
        thisExp.timestampOnFlip(win, 'thisRow.t', format=globalClock.format)
        # abbreviate parameter names if possible (e.g. rgb = thisTransfer_instructions_loop.rgb)
        if thisTransfer_instructions_loop != None:
            for paramName in thisTransfer_instructions_loop:
                globals()[paramName] = thisTransfer_instructions_loop[paramName]
        
        # --- Prepare to start Routine "transfer_instructions1" ---
        # create an object to store info about Routine transfer_instructions1
        transfer_instructions1 = data.Routine(
            name='transfer_instructions1',
            components=[tinstr1_1, transfer_cont1],
        )
        transfer_instructions1.status = NOT_STARTED
        continueRoutine = True
        # update component parameters for each repeat
        # create starting attributes for transfer_cont1
        transfer_cont1.keys = []
        transfer_cont1.rt = []
        _transfer_cont1_allKeys = []
        # store start times for transfer_instructions1
        transfer_instructions1.tStartRefresh = win.getFutureFlipTime(clock=globalClock)
        transfer_instructions1.tStart = globalClock.getTime(format='float')
        transfer_instructions1.status = STARTED
        thisExp.addData('transfer_instructions1.started', transfer_instructions1.tStart)
        transfer_instructions1.maxDuration = None
        # keep track of which components have finished
        transfer_instructions1Components = transfer_instructions1.components
        for thisComponent in transfer_instructions1.components:
            thisComponent.tStart = None
            thisComponent.tStop = None
            thisComponent.tStartRefresh = None
            thisComponent.tStopRefresh = None
            if hasattr(thisComponent, 'status'):
                thisComponent.status = NOT_STARTED
        # reset timers
        t = 0
        _timeToFirstFrame = win.getFutureFlipTime(clock="now")
        frameN = -1
        
        # --- Run Routine "transfer_instructions1" ---
        # if trial has changed, end Routine now
        if isinstance(transfer_instructions_loop, data.TrialHandler2) and thisTransfer_instructions_loop.thisN != transfer_instructions_loop.thisTrial.thisN:
            continueRoutine = False
        transfer_instructions1.forceEnded = routineForceEnded = not continueRoutine
        while continueRoutine:
            # get current time
            t = routineTimer.getTime()
            tThisFlip = win.getFutureFlipTime(clock=routineTimer)
            tThisFlipGlobal = win.getFutureFlipTime(clock=None)
            frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
            # update/draw components on each frame
            
            # *tinstr1_1* updates
            
            # if tinstr1_1 is starting this frame...
            if tinstr1_1.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                # keep track of start time/frame for later
                tinstr1_1.frameNStart = frameN  # exact frame index
                tinstr1_1.tStart = t  # local t and not account for scr refresh
                tinstr1_1.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(tinstr1_1, 'tStartRefresh')  # time at next scr refresh
                # add timestamp to datafile
                thisExp.timestampOnFlip(win, 'tinstr1_1.started')
                # update status
                tinstr1_1.status = STARTED
                tinstr1_1.setAutoDraw(True)
            
            # if tinstr1_1 is active this frame...
            if tinstr1_1.status == STARTED:
                # update params
                pass
            
            # *transfer_cont1* updates
            waitOnFlip = False
            
            # if transfer_cont1 is starting this frame...
            if transfer_cont1.status == NOT_STARTED and tThisFlip >= .33-frameTolerance:
                # keep track of start time/frame for later
                transfer_cont1.frameNStart = frameN  # exact frame index
                transfer_cont1.tStart = t  # local t and not account for scr refresh
                transfer_cont1.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(transfer_cont1, 'tStartRefresh')  # time at next scr refresh
                # add timestamp to datafile
                thisExp.timestampOnFlip(win, 'transfer_cont1.started')
                # update status
                transfer_cont1.status = STARTED
                # keyboard checking is just starting
                waitOnFlip = True
                win.callOnFlip(transfer_cont1.clock.reset)  # t=0 on next screen flip
                win.callOnFlip(transfer_cont1.clearEvents, eventType='keyboard')  # clear events on next screen flip
            if transfer_cont1.status == STARTED and not waitOnFlip:
                theseKeys = transfer_cont1.getKeys(keyList=['space'], ignoreKeys=["escape"], waitRelease=False)
                _transfer_cont1_allKeys.extend(theseKeys)
                if len(_transfer_cont1_allKeys):
                    transfer_cont1.keys = _transfer_cont1_allKeys[-1].name  # just the last key pressed
                    transfer_cont1.rt = _transfer_cont1_allKeys[-1].rt
                    transfer_cont1.duration = _transfer_cont1_allKeys[-1].duration
                    # a response ends the routine
                    continueRoutine = False
            
            # check for quit (typically the Esc key)
            if defaultKeyboard.getKeys(keyList=["escape"]):
                thisExp.status = FINISHED
            if thisExp.status == FINISHED or endExpNow:
                endExperiment(thisExp, win=win)
                return
            # pause experiment here if requested
            if thisExp.status == PAUSED:
                pauseExperiment(
                    thisExp=thisExp, 
                    win=win, 
                    timers=[routineTimer], 
                    playbackComponents=[]
                )
                # skip the frame we paused on
                continue
            
            # check if all components have finished
            if not continueRoutine:  # a component has requested a forced-end of Routine
                transfer_instructions1.forceEnded = routineForceEnded = True
                break
            continueRoutine = False  # will revert to True if at least one component still running
            for thisComponent in transfer_instructions1.components:
                if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                    continueRoutine = True
                    break  # at least one component has not yet finished
            
            # refresh the screen
            if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
                win.flip()
        
        # --- Ending Routine "transfer_instructions1" ---
        for thisComponent in transfer_instructions1.components:
            if hasattr(thisComponent, "setAutoDraw"):
                thisComponent.setAutoDraw(False)
        # store stop times for transfer_instructions1
        transfer_instructions1.tStop = globalClock.getTime(format='float')
        transfer_instructions1.tStopRefresh = tThisFlipGlobal
        thisExp.addData('transfer_instructions1.stopped', transfer_instructions1.tStop)
        # check responses
        if transfer_cont1.keys in ['', [], None]:  # No response was made
            transfer_cont1.keys = None
        transfer_instructions_loop.addData('transfer_cont1.keys',transfer_cont1.keys)
        if transfer_cont1.keys != None:  # we had a response
            transfer_instructions_loop.addData('transfer_cont1.rt', transfer_cont1.rt)
            transfer_instructions_loop.addData('transfer_cont1.duration', transfer_cont1.duration)
        # the Routine "transfer_instructions1" was not non-slip safe, so reset the non-slip timer
        routineTimer.reset()
        
        # --- Prepare to start Routine "transfer_instructions2" ---
        # create an object to store info about Routine transfer_instructions2
        transfer_instructions2 = data.Routine(
            name='transfer_instructions2',
            components=[tinstr2_1, tinstr2_2, tinstr2_3, tinstr2_4, tinstr2_5, transfer_cont2],
        )
        transfer_instructions2.status = NOT_STARTED
        continueRoutine = True
        # update component parameters for each repeat
        # create starting attributes for transfer_cont2
        transfer_cont2.keys = []
        transfer_cont2.rt = []
        _transfer_cont2_allKeys = []
        # store start times for transfer_instructions2
        transfer_instructions2.tStartRefresh = win.getFutureFlipTime(clock=globalClock)
        transfer_instructions2.tStart = globalClock.getTime(format='float')
        transfer_instructions2.status = STARTED
        thisExp.addData('transfer_instructions2.started', transfer_instructions2.tStart)
        transfer_instructions2.maxDuration = None
        # keep track of which components have finished
        transfer_instructions2Components = transfer_instructions2.components
        for thisComponent in transfer_instructions2.components:
            thisComponent.tStart = None
            thisComponent.tStop = None
            thisComponent.tStartRefresh = None
            thisComponent.tStopRefresh = None
            if hasattr(thisComponent, 'status'):
                thisComponent.status = NOT_STARTED
        # reset timers
        t = 0
        _timeToFirstFrame = win.getFutureFlipTime(clock="now")
        frameN = -1
        
        # --- Run Routine "transfer_instructions2" ---
        # if trial has changed, end Routine now
        if isinstance(transfer_instructions_loop, data.TrialHandler2) and thisTransfer_instructions_loop.thisN != transfer_instructions_loop.thisTrial.thisN:
            continueRoutine = False
        transfer_instructions2.forceEnded = routineForceEnded = not continueRoutine
        while continueRoutine:
            # get current time
            t = routineTimer.getTime()
            tThisFlip = win.getFutureFlipTime(clock=routineTimer)
            tThisFlipGlobal = win.getFutureFlipTime(clock=None)
            frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
            # update/draw components on each frame
            
            # *tinstr2_1* updates
            
            # if tinstr2_1 is starting this frame...
            if tinstr2_1.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                # keep track of start time/frame for later
                tinstr2_1.frameNStart = frameN  # exact frame index
                tinstr2_1.tStart = t  # local t and not account for scr refresh
                tinstr2_1.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(tinstr2_1, 'tStartRefresh')  # time at next scr refresh
                # add timestamp to datafile
                thisExp.timestampOnFlip(win, 'tinstr2_1.started')
                # update status
                tinstr2_1.status = STARTED
                tinstr2_1.setAutoDraw(True)
            
            # if tinstr2_1 is active this frame...
            if tinstr2_1.status == STARTED:
                # update params
                pass
            
            # *tinstr2_2* updates
            
            # if tinstr2_2 is starting this frame...
            if tinstr2_2.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                # keep track of start time/frame for later
                tinstr2_2.frameNStart = frameN  # exact frame index
                tinstr2_2.tStart = t  # local t and not account for scr refresh
                tinstr2_2.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(tinstr2_2, 'tStartRefresh')  # time at next scr refresh
                # add timestamp to datafile
                thisExp.timestampOnFlip(win, 'tinstr2_2.started')
                # update status
                tinstr2_2.status = STARTED
                tinstr2_2.setAutoDraw(True)
            
            # if tinstr2_2 is active this frame...
            if tinstr2_2.status == STARTED:
                # update params
                pass
            
            # *tinstr2_3* updates
            
            # if tinstr2_3 is starting this frame...
            if tinstr2_3.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                # keep track of start time/frame for later
                tinstr2_3.frameNStart = frameN  # exact frame index
                tinstr2_3.tStart = t  # local t and not account for scr refresh
                tinstr2_3.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(tinstr2_3, 'tStartRefresh')  # time at next scr refresh
                # add timestamp to datafile
                thisExp.timestampOnFlip(win, 'tinstr2_3.started')
                # update status
                tinstr2_3.status = STARTED
                tinstr2_3.setAutoDraw(True)
            
            # if tinstr2_3 is active this frame...
            if tinstr2_3.status == STARTED:
                # update params
                pass
            
            # *tinstr2_4* updates
            
            # if tinstr2_4 is starting this frame...
            if tinstr2_4.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                # keep track of start time/frame for later
                tinstr2_4.frameNStart = frameN  # exact frame index
                tinstr2_4.tStart = t  # local t and not account for scr refresh
                tinstr2_4.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(tinstr2_4, 'tStartRefresh')  # time at next scr refresh
                # add timestamp to datafile
                thisExp.timestampOnFlip(win, 'tinstr2_4.started')
                # update status
                tinstr2_4.status = STARTED
                tinstr2_4.setAutoDraw(True)
            
            # if tinstr2_4 is active this frame...
            if tinstr2_4.status == STARTED:
                # update params
                pass
            
            # *tinstr2_5* updates
            
            # if tinstr2_5 is starting this frame...
            if tinstr2_5.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                # keep track of start time/frame for later
                tinstr2_5.frameNStart = frameN  # exact frame index
                tinstr2_5.tStart = t  # local t and not account for scr refresh
                tinstr2_5.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(tinstr2_5, 'tStartRefresh')  # time at next scr refresh
                # add timestamp to datafile
                thisExp.timestampOnFlip(win, 'tinstr2_5.started')
                # update status
                tinstr2_5.status = STARTED
                tinstr2_5.setAutoDraw(True)
            
            # if tinstr2_5 is active this frame...
            if tinstr2_5.status == STARTED:
                # update params
                pass
            
            # *transfer_cont2* updates
            waitOnFlip = False
            
            # if transfer_cont2 is starting this frame...
            if transfer_cont2.status == NOT_STARTED and tThisFlip >= 4.0-frameTolerance:
                # keep track of start time/frame for later
                transfer_cont2.frameNStart = frameN  # exact frame index
                transfer_cont2.tStart = t  # local t and not account for scr refresh
                transfer_cont2.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(transfer_cont2, 'tStartRefresh')  # time at next scr refresh
                # add timestamp to datafile
                thisExp.timestampOnFlip(win, 'transfer_cont2.started')
                # update status
                transfer_cont2.status = STARTED
                # keyboard checking is just starting
                waitOnFlip = True
                win.callOnFlip(transfer_cont2.clock.reset)  # t=0 on next screen flip
                win.callOnFlip(transfer_cont2.clearEvents, eventType='keyboard')  # clear events on next screen flip
            if transfer_cont2.status == STARTED and not waitOnFlip:
                theseKeys = transfer_cont2.getKeys(keyList=['space'], ignoreKeys=["escape"], waitRelease=False)
                _transfer_cont2_allKeys.extend(theseKeys)
                if len(_transfer_cont2_allKeys):
                    transfer_cont2.keys = _transfer_cont2_allKeys[-1].name  # just the last key pressed
                    transfer_cont2.rt = _transfer_cont2_allKeys[-1].rt
                    transfer_cont2.duration = _transfer_cont2_allKeys[-1].duration
                    # a response ends the routine
                    continueRoutine = False
            
            # check for quit (typically the Esc key)
            if defaultKeyboard.getKeys(keyList=["escape"]):
                thisExp.status = FINISHED
            if thisExp.status == FINISHED or endExpNow:
                endExperiment(thisExp, win=win)
                return
            # pause experiment here if requested
            if thisExp.status == PAUSED:
                pauseExperiment(
                    thisExp=thisExp, 
                    win=win, 
                    timers=[routineTimer], 
                    playbackComponents=[]
                )
                # skip the frame we paused on
                continue
            
            # check if all components have finished
            if not continueRoutine:  # a component has requested a forced-end of Routine
                transfer_instructions2.forceEnded = routineForceEnded = True
                break
            continueRoutine = False  # will revert to True if at least one component still running
            for thisComponent in transfer_instructions2.components:
                if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                    continueRoutine = True
                    break  # at least one component has not yet finished
            
            # refresh the screen
            if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
                win.flip()
        
        # --- Ending Routine "transfer_instructions2" ---
        for thisComponent in transfer_instructions2.components:
            if hasattr(thisComponent, "setAutoDraw"):
                thisComponent.setAutoDraw(False)
        # store stop times for transfer_instructions2
        transfer_instructions2.tStop = globalClock.getTime(format='float')
        transfer_instructions2.tStopRefresh = tThisFlipGlobal
        thisExp.addData('transfer_instructions2.stopped', transfer_instructions2.tStop)
        # check responses
        if transfer_cont2.keys in ['', [], None]:  # No response was made
            transfer_cont2.keys = None
        transfer_instructions_loop.addData('transfer_cont2.keys',transfer_cont2.keys)
        if transfer_cont2.keys != None:  # we had a response
            transfer_instructions_loop.addData('transfer_cont2.rt', transfer_cont2.rt)
            transfer_instructions_loop.addData('transfer_cont2.duration', transfer_cont2.duration)
        # the Routine "transfer_instructions2" was not non-slip safe, so reset the non-slip timer
        routineTimer.reset()
        
        # --- Prepare to start Routine "transfer_instructions3" ---
        # create an object to store info about Routine transfer_instructions3
        transfer_instructions3 = data.Routine(
            name='transfer_instructions3',
            components=[tinstr3_1, transfer_cont3],
        )
        transfer_instructions3.status = NOT_STARTED
        continueRoutine = True
        # update component parameters for each repeat
        # create starting attributes for transfer_cont3
        transfer_cont3.keys = []
        transfer_cont3.rt = []
        _transfer_cont3_allKeys = []
        # store start times for transfer_instructions3
        transfer_instructions3.tStartRefresh = win.getFutureFlipTime(clock=globalClock)
        transfer_instructions3.tStart = globalClock.getTime(format='float')
        transfer_instructions3.status = STARTED
        thisExp.addData('transfer_instructions3.started', transfer_instructions3.tStart)
        transfer_instructions3.maxDuration = None
        # keep track of which components have finished
        transfer_instructions3Components = transfer_instructions3.components
        for thisComponent in transfer_instructions3.components:
            thisComponent.tStart = None
            thisComponent.tStop = None
            thisComponent.tStartRefresh = None
            thisComponent.tStopRefresh = None
            if hasattr(thisComponent, 'status'):
                thisComponent.status = NOT_STARTED
        # reset timers
        t = 0
        _timeToFirstFrame = win.getFutureFlipTime(clock="now")
        frameN = -1
        
        # --- Run Routine "transfer_instructions3" ---
        # if trial has changed, end Routine now
        if isinstance(transfer_instructions_loop, data.TrialHandler2) and thisTransfer_instructions_loop.thisN != transfer_instructions_loop.thisTrial.thisN:
            continueRoutine = False
        transfer_instructions3.forceEnded = routineForceEnded = not continueRoutine
        while continueRoutine:
            # get current time
            t = routineTimer.getTime()
            tThisFlip = win.getFutureFlipTime(clock=routineTimer)
            tThisFlipGlobal = win.getFutureFlipTime(clock=None)
            frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
            # update/draw components on each frame
            
            # *tinstr3_1* updates
            
            # if tinstr3_1 is starting this frame...
            if tinstr3_1.status == NOT_STARTED and tThisFlip >= 0.00-frameTolerance:
                # keep track of start time/frame for later
                tinstr3_1.frameNStart = frameN  # exact frame index
                tinstr3_1.tStart = t  # local t and not account for scr refresh
                tinstr3_1.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(tinstr3_1, 'tStartRefresh')  # time at next scr refresh
                # add timestamp to datafile
                thisExp.timestampOnFlip(win, 'tinstr3_1.started')
                # update status
                tinstr3_1.status = STARTED
                tinstr3_1.setAutoDraw(True)
            
            # if tinstr3_1 is active this frame...
            if tinstr3_1.status == STARTED:
                # update params
                pass
            
            # *transfer_cont3* updates
            waitOnFlip = False
            
            # if transfer_cont3 is starting this frame...
            if transfer_cont3.status == NOT_STARTED and tThisFlip >= 0.5-frameTolerance:
                # keep track of start time/frame for later
                transfer_cont3.frameNStart = frameN  # exact frame index
                transfer_cont3.tStart = t  # local t and not account for scr refresh
                transfer_cont3.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(transfer_cont3, 'tStartRefresh')  # time at next scr refresh
                # add timestamp to datafile
                thisExp.timestampOnFlip(win, 'transfer_cont3.started')
                # update status
                transfer_cont3.status = STARTED
                # keyboard checking is just starting
                waitOnFlip = True
                win.callOnFlip(transfer_cont3.clock.reset)  # t=0 on next screen flip
                win.callOnFlip(transfer_cont3.clearEvents, eventType='keyboard')  # clear events on next screen flip
            if transfer_cont3.status == STARTED and not waitOnFlip:
                theseKeys = transfer_cont3.getKeys(keyList=['space'], ignoreKeys=["escape"], waitRelease=False)
                _transfer_cont3_allKeys.extend(theseKeys)
                if len(_transfer_cont3_allKeys):
                    transfer_cont3.keys = _transfer_cont3_allKeys[-1].name  # just the last key pressed
                    transfer_cont3.rt = _transfer_cont3_allKeys[-1].rt
                    transfer_cont3.duration = _transfer_cont3_allKeys[-1].duration
                    # a response ends the routine
                    continueRoutine = False
            
            # check for quit (typically the Esc key)
            if defaultKeyboard.getKeys(keyList=["escape"]):
                thisExp.status = FINISHED
            if thisExp.status == FINISHED or endExpNow:
                endExperiment(thisExp, win=win)
                return
            # pause experiment here if requested
            if thisExp.status == PAUSED:
                pauseExperiment(
                    thisExp=thisExp, 
                    win=win, 
                    timers=[routineTimer], 
                    playbackComponents=[]
                )
                # skip the frame we paused on
                continue
            
            # check if all components have finished
            if not continueRoutine:  # a component has requested a forced-end of Routine
                transfer_instructions3.forceEnded = routineForceEnded = True
                break
            continueRoutine = False  # will revert to True if at least one component still running
            for thisComponent in transfer_instructions3.components:
                if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                    continueRoutine = True
                    break  # at least one component has not yet finished
            
            # refresh the screen
            if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
                win.flip()
        
        # --- Ending Routine "transfer_instructions3" ---
        for thisComponent in transfer_instructions3.components:
            if hasattr(thisComponent, "setAutoDraw"):
                thisComponent.setAutoDraw(False)
        # store stop times for transfer_instructions3
        transfer_instructions3.tStop = globalClock.getTime(format='float')
        transfer_instructions3.tStopRefresh = tThisFlipGlobal
        thisExp.addData('transfer_instructions3.stopped', transfer_instructions3.tStop)
        # check responses
        if transfer_cont3.keys in ['', [], None]:  # No response was made
            transfer_cont3.keys = None
        transfer_instructions_loop.addData('transfer_cont3.keys',transfer_cont3.keys)
        if transfer_cont3.keys != None:  # we had a response
            transfer_instructions_loop.addData('transfer_cont3.rt', transfer_cont3.rt)
            transfer_instructions_loop.addData('transfer_cont3.duration', transfer_cont3.duration)
        # the Routine "transfer_instructions3" was not non-slip safe, so reset the non-slip timer
        routineTimer.reset()
    # completed 0.0 repeats of 'transfer_instructions_loop'
    
    
    # set up handler to look after randomisation of conditions etc
    transfer_trials = data.TrialHandler2(
        name='transfer_trials',
        nReps=3.0, 
        method='random', 
        extraInfo=expInfo, 
        originPath=-1, 
        trialList=data.importConditions(transfer_condition_file), 
        seed=None, 
    )
    thisExp.addLoop(transfer_trials)  # add the loop to the experiment
    thisTransfer_trial = transfer_trials.trialList[0]  # so we can initialise stimuli with some values
    # abbreviate parameter names if possible (e.g. rgb = thisTransfer_trial.rgb)
    if thisTransfer_trial != None:
        for paramName in thisTransfer_trial:
            globals()[paramName] = thisTransfer_trial[paramName]
    if thisSession is not None:
        # if running in a Session with a Liaison client, send data up to now
        thisSession.sendExperimentData()
    
    for thisTransfer_trial in transfer_trials:
        currentLoop = transfer_trials
        thisExp.timestampOnFlip(win, 'thisRow.t', format=globalClock.format)
        if thisSession is not None:
            # if running in a Session with a Liaison client, send data up to now
            thisSession.sendExperimentData()
        # abbreviate parameter names if possible (e.g. rgb = thisTransfer_trial.rgb)
        if thisTransfer_trial != None:
            for paramName in thisTransfer_trial:
                globals()[paramName] = thisTransfer_trial[paramName]
        
        # --- Prepare to start Routine "shuffle_trial_stimuli" ---
        # create an object to store info about Routine shuffle_trial_stimuli
        shuffle_trial_stimuli = data.Routine(
            name='shuffle_trial_stimuli',
            components=[],
        )
        shuffle_trial_stimuli.status = NOT_STARTED
        continueRoutine = True
        # update component parameters for each repeat
        # Run 'Begin Routine' code from shuffle_stim
        import numpy as np
        from psychopy import logging
        if(set_type=='trinary'):
            stim_all_psy_unshuffled=np.array([stim_1_psy,stim_2_psy,stim_3_psy])
            stim_all_phy_unshuffled=np.array([stim_1_phy,stim_2_phy,stim_3_phy])
            stim_order=np.random.permutation(np.arange(3))
            stim_1_phy_shuffled=stim_all_phy_unshuffled[stim_order[0]]
            stim_2_phy_shuffled=stim_all_phy_unshuffled[stim_order[1]]
            stim_3_phy_shuffled=stim_all_phy_unshuffled[stim_order[2]]
            stim_1_psy_shuffled=stim_all_psy_unshuffled[stim_order[0]]
            stim_2_psy_shuffled=stim_all_psy_unshuffled[stim_order[1]]
            stim_3_psy_shuffled=stim_all_psy_unshuffled[stim_order[2]]
            if(stim_correct_2==0):
                key_correct_transfer_trinary_2='NA'
                if(stim_1_psy_shuffled==stim_correct_1):
                    key_correct_transfer_trinary_1='j'
                elif(stim_2_psy_shuffled==stim_correct_1):
                    key_correct_transfer_trinary_1='k'
                elif(stim_3_psy_shuffled==stim_correct_1):
                    key_correct_transfer_trinary_1='l'
            else:
                if(stim_1_psy_shuffled==stim_correct_1):
                    key_correct_transfer_trinary_1='j'
                elif(stim_2_psy_shuffled==stim_correct_1):
                    key_correct_transfer_trinary_1='k'
                elif(stim_3_psy_shuffled==stim_correct_1):
                    key_correct_transfer_trinary_1='l'
                if(stim_1_psy_shuffled==stim_correct_2):
                    key_correct_transfer_trinary_2='j'
                elif(stim_2_psy_shuffled==stim_correct_2):
                    key_correct_transfer_trinary_2='k'
                elif(stim_3_psy_shuffled==stim_correct_2):
                    key_correct_transfer_trinary_2='l'
            key_correct_transfer_binary_1='NA'
            key_correct_transfer_binary_2='NA'
            do_trinary=True
            do_binary=False
        elif(set_type=='binary'):
            stim_all_psy_unshuffled=np.array([stim_1_psy,stim_2_psy])
            stim_all_phy_unshuffled=np.array([stim_1_phy,stim_2_phy])
            stim_order=np.random.permutation(np.arange(2))
            stim_1_phy_shuffled=stim_all_phy_unshuffled[stim_order[0]]
            stim_2_phy_shuffled=stim_all_phy_unshuffled[stim_order[1]]
            stim_1_psy_shuffled=stim_all_psy_unshuffled[stim_order[0]]
            stim_2_psy_shuffled=stim_all_psy_unshuffled[stim_order[1]]
            if(stim_correct_2==0):
                key_correct_transfer_binary_2='NA'
                if(stim_1_psy_shuffled==stim_correct_1):
                    key_correct_transfer_binary_1='j'
                elif(stim_2_psy_shuffled==stim_correct_1):
                    key_correct_transfer_binary_1='k'
            else:
                key_correct_transfer_binary_1='j'
                key_correct_transfer_binary_2='k'
            do_trinary=False
            do_binary=True
            key_correct_transfer_trinary_1='NA'
            key_correct_transfer_trinary_2='NA'
            stim_3_phy_shuffled=0
            stim_3_psy_shuffled=0
        
        
        
        # store start times for shuffle_trial_stimuli
        shuffle_trial_stimuli.tStartRefresh = win.getFutureFlipTime(clock=globalClock)
        shuffle_trial_stimuli.tStart = globalClock.getTime(format='float')
        shuffle_trial_stimuli.status = STARTED
        thisExp.addData('shuffle_trial_stimuli.started', shuffle_trial_stimuli.tStart)
        shuffle_trial_stimuli.maxDuration = None
        # keep track of which components have finished
        shuffle_trial_stimuliComponents = shuffle_trial_stimuli.components
        for thisComponent in shuffle_trial_stimuli.components:
            thisComponent.tStart = None
            thisComponent.tStop = None
            thisComponent.tStartRefresh = None
            thisComponent.tStopRefresh = None
            if hasattr(thisComponent, 'status'):
                thisComponent.status = NOT_STARTED
        # reset timers
        t = 0
        _timeToFirstFrame = win.getFutureFlipTime(clock="now")
        frameN = -1
        
        # --- Run Routine "shuffle_trial_stimuli" ---
        # if trial has changed, end Routine now
        if isinstance(transfer_trials, data.TrialHandler2) and thisTransfer_trial.thisN != transfer_trials.thisTrial.thisN:
            continueRoutine = False
        shuffle_trial_stimuli.forceEnded = routineForceEnded = not continueRoutine
        while continueRoutine:
            # get current time
            t = routineTimer.getTime()
            tThisFlip = win.getFutureFlipTime(clock=routineTimer)
            tThisFlipGlobal = win.getFutureFlipTime(clock=None)
            frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
            # update/draw components on each frame
            
            # check for quit (typically the Esc key)
            if defaultKeyboard.getKeys(keyList=["escape"]):
                thisExp.status = FINISHED
            if thisExp.status == FINISHED or endExpNow:
                endExperiment(thisExp, win=win)
                return
            # pause experiment here if requested
            if thisExp.status == PAUSED:
                pauseExperiment(
                    thisExp=thisExp, 
                    win=win, 
                    timers=[routineTimer], 
                    playbackComponents=[]
                )
                # skip the frame we paused on
                continue
            
            # check if all components have finished
            if not continueRoutine:  # a component has requested a forced-end of Routine
                shuffle_trial_stimuli.forceEnded = routineForceEnded = True
                break
            continueRoutine = False  # will revert to True if at least one component still running
            for thisComponent in shuffle_trial_stimuli.components:
                if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                    continueRoutine = True
                    break  # at least one component has not yet finished
            
            # refresh the screen
            if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
                win.flip()
        
        # --- Ending Routine "shuffle_trial_stimuli" ---
        for thisComponent in shuffle_trial_stimuli.components:
            if hasattr(thisComponent, "setAutoDraw"):
                thisComponent.setAutoDraw(False)
        # store stop times for shuffle_trial_stimuli
        shuffle_trial_stimuli.tStop = globalClock.getTime(format='float')
        shuffle_trial_stimuli.tStopRefresh = tThisFlipGlobal
        thisExp.addData('shuffle_trial_stimuli.stopped', shuffle_trial_stimuli.tStop)
        # Run 'End Routine' code from shuffle_stim
        thisExp.addData('stim_order',stim_order)
        thisExp.addData('stim_1_phy_shuffled',stim_1_phy_shuffled)
        thisExp.addData('stim_2_phy_shuffled',stim_2_phy_shuffled)
        thisExp.addData('stim_3_phy_shuffled',stim_3_phy_shuffled)
        thisExp.addData('stim_1_psy_shuffled',stim_1_psy_shuffled)
        thisExp.addData('stim_2_psy_shuffled',stim_2_psy_shuffled)
        thisExp.addData('stim_3_psy_shuffled',stim_3_psy_shuffled)
        thisExp.addData('key_correct_transfer_trinary_1',key_correct_transfer_trinary_1)
        thisExp.addData('key_correct_transfer_trinary_2',key_correct_transfer_trinary_2)
        thisExp.addData('key_correct_transfer_binary_1',key_correct_transfer_binary_1)
        thisExp.addData('key_correct_transfer_binary_2',key_correct_transfer_binary_2)
        
        # the Routine "shuffle_trial_stimuli" was not non-slip safe, so reset the non-slip timer
        routineTimer.reset()
        
        # set up handler to look after randomisation of conditions etc
        transfer_trinary_dummy = data.TrialHandler2(
            name='transfer_trinary_dummy',
            nReps=do_trinary, 
            method='random', 
            extraInfo=expInfo, 
            originPath=-1, 
            trialList=[None], 
            seed=None, 
        )
        thisExp.addLoop(transfer_trinary_dummy)  # add the loop to the experiment
        thisTransfer_trinary_dummy = transfer_trinary_dummy.trialList[0]  # so we can initialise stimuli with some values
        # abbreviate parameter names if possible (e.g. rgb = thisTransfer_trinary_dummy.rgb)
        if thisTransfer_trinary_dummy != None:
            for paramName in thisTransfer_trinary_dummy:
                globals()[paramName] = thisTransfer_trinary_dummy[paramName]
        if thisSession is not None:
            # if running in a Session with a Liaison client, send data up to now
            thisSession.sendExperimentData()
        
        for thisTransfer_trinary_dummy in transfer_trinary_dummy:
            currentLoop = transfer_trinary_dummy
            thisExp.timestampOnFlip(win, 'thisRow.t', format=globalClock.format)
            if thisSession is not None:
                # if running in a Session with a Liaison client, send data up to now
                thisSession.sendExperimentData()
            # abbreviate parameter names if possible (e.g. rgb = thisTransfer_trinary_dummy.rgb)
            if thisTransfer_trinary_dummy != None:
                for paramName in thisTransfer_trinary_dummy:
                    globals()[paramName] = thisTransfer_trinary_dummy[paramName]
            
            # --- Prepare to start Routine "transfer_trial_trinary" ---
            # create an object to store info about Routine transfer_trial_trinary
            transfer_trial_trinary = data.Routine(
                name='transfer_trial_trinary',
                components=[line_1_trinary, line_2_trinary, line_3_trinary, key_resp_transfer_trinary, line_1_trinary_label, line_2_trinary_label, line_3_trinary_label, transfer_trinary_prompt],
            )
            transfer_trial_trinary.status = NOT_STARTED
            continueRoutine = True
            # update component parameters for each repeat
            line_1_trinary.setPos((-dist_btw, 0))
            line_1_trinary.setSize((line_width,stim_1_phy_shuffled))
            line_2_trinary.setPos((0,0))
            line_2_trinary.setSize((line_width,stim_2_phy_shuffled))
            line_3_trinary.setPos((dist_btw, 0))
            line_3_trinary.setSize((line_width,stim_3_phy_shuffled))
            # create starting attributes for key_resp_transfer_trinary
            key_resp_transfer_trinary.keys = []
            key_resp_transfer_trinary.rt = []
            _key_resp_transfer_trinary_allKeys = []
            # store start times for transfer_trial_trinary
            transfer_trial_trinary.tStartRefresh = win.getFutureFlipTime(clock=globalClock)
            transfer_trial_trinary.tStart = globalClock.getTime(format='float')
            transfer_trial_trinary.status = STARTED
            thisExp.addData('transfer_trial_trinary.started', transfer_trial_trinary.tStart)
            transfer_trial_trinary.maxDuration = None
            # keep track of which components have finished
            transfer_trial_trinaryComponents = transfer_trial_trinary.components
            for thisComponent in transfer_trial_trinary.components:
                thisComponent.tStart = None
                thisComponent.tStop = None
                thisComponent.tStartRefresh = None
                thisComponent.tStopRefresh = None
                if hasattr(thisComponent, 'status'):
                    thisComponent.status = NOT_STARTED
            # reset timers
            t = 0
            _timeToFirstFrame = win.getFutureFlipTime(clock="now")
            frameN = -1
            
            # --- Run Routine "transfer_trial_trinary" ---
            # if trial has changed, end Routine now
            if isinstance(transfer_trinary_dummy, data.TrialHandler2) and thisTransfer_trinary_dummy.thisN != transfer_trinary_dummy.thisTrial.thisN:
                continueRoutine = False
            transfer_trial_trinary.forceEnded = routineForceEnded = not continueRoutine
            while continueRoutine:
                # get current time
                t = routineTimer.getTime()
                tThisFlip = win.getFutureFlipTime(clock=routineTimer)
                tThisFlipGlobal = win.getFutureFlipTime(clock=None)
                frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
                # update/draw components on each frame
                
                # *line_1_trinary* updates
                
                # if line_1_trinary is starting this frame...
                if line_1_trinary.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                    # keep track of start time/frame for later
                    line_1_trinary.frameNStart = frameN  # exact frame index
                    line_1_trinary.tStart = t  # local t and not account for scr refresh
                    line_1_trinary.tStartRefresh = tThisFlipGlobal  # on global time
                    win.timeOnFlip(line_1_trinary, 'tStartRefresh')  # time at next scr refresh
                    # add timestamp to datafile
                    thisExp.timestampOnFlip(win, 'line_1_trinary.started')
                    # update status
                    line_1_trinary.status = STARTED
                    line_1_trinary.setAutoDraw(True)
                
                # if line_1_trinary is active this frame...
                if line_1_trinary.status == STARTED:
                    # update params
                    pass
                
                # *line_2_trinary* updates
                
                # if line_2_trinary is starting this frame...
                if line_2_trinary.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                    # keep track of start time/frame for later
                    line_2_trinary.frameNStart = frameN  # exact frame index
                    line_2_trinary.tStart = t  # local t and not account for scr refresh
                    line_2_trinary.tStartRefresh = tThisFlipGlobal  # on global time
                    win.timeOnFlip(line_2_trinary, 'tStartRefresh')  # time at next scr refresh
                    # add timestamp to datafile
                    thisExp.timestampOnFlip(win, 'line_2_trinary.started')
                    # update status
                    line_2_trinary.status = STARTED
                    line_2_trinary.setAutoDraw(True)
                
                # if line_2_trinary is active this frame...
                if line_2_trinary.status == STARTED:
                    # update params
                    pass
                
                # *line_3_trinary* updates
                
                # if line_3_trinary is starting this frame...
                if line_3_trinary.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                    # keep track of start time/frame for later
                    line_3_trinary.frameNStart = frameN  # exact frame index
                    line_3_trinary.tStart = t  # local t and not account for scr refresh
                    line_3_trinary.tStartRefresh = tThisFlipGlobal  # on global time
                    win.timeOnFlip(line_3_trinary, 'tStartRefresh')  # time at next scr refresh
                    # add timestamp to datafile
                    thisExp.timestampOnFlip(win, 'line_3_trinary.started')
                    # update status
                    line_3_trinary.status = STARTED
                    line_3_trinary.setAutoDraw(True)
                
                # if line_3_trinary is active this frame...
                if line_3_trinary.status == STARTED:
                    # update params
                    pass
                
                # *key_resp_transfer_trinary* updates
                waitOnFlip = False
                
                # if key_resp_transfer_trinary is starting this frame...
                if key_resp_transfer_trinary.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                    # keep track of start time/frame for later
                    key_resp_transfer_trinary.frameNStart = frameN  # exact frame index
                    key_resp_transfer_trinary.tStart = t  # local t and not account for scr refresh
                    key_resp_transfer_trinary.tStartRefresh = tThisFlipGlobal  # on global time
                    win.timeOnFlip(key_resp_transfer_trinary, 'tStartRefresh')  # time at next scr refresh
                    # add timestamp to datafile
                    thisExp.timestampOnFlip(win, 'key_resp_transfer_trinary.started')
                    # update status
                    key_resp_transfer_trinary.status = STARTED
                    # keyboard checking is just starting
                    waitOnFlip = True
                    win.callOnFlip(key_resp_transfer_trinary.clock.reset)  # t=0 on next screen flip
                    win.callOnFlip(key_resp_transfer_trinary.clearEvents, eventType='keyboard')  # clear events on next screen flip
                if key_resp_transfer_trinary.status == STARTED and not waitOnFlip:
                    theseKeys = key_resp_transfer_trinary.getKeys(keyList=['j','k','l'], ignoreKeys=["escape"], waitRelease=False)
                    _key_resp_transfer_trinary_allKeys.extend(theseKeys)
                    if len(_key_resp_transfer_trinary_allKeys):
                        key_resp_transfer_trinary.keys = _key_resp_transfer_trinary_allKeys[-1].name  # just the last key pressed
                        key_resp_transfer_trinary.rt = _key_resp_transfer_trinary_allKeys[-1].rt
                        key_resp_transfer_trinary.duration = _key_resp_transfer_trinary_allKeys[-1].duration
                        # a response ends the routine
                        continueRoutine = False
                
                # *line_1_trinary_label* updates
                
                # if line_1_trinary_label is starting this frame...
                if line_1_trinary_label.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                    # keep track of start time/frame for later
                    line_1_trinary_label.frameNStart = frameN  # exact frame index
                    line_1_trinary_label.tStart = t  # local t and not account for scr refresh
                    line_1_trinary_label.tStartRefresh = tThisFlipGlobal  # on global time
                    win.timeOnFlip(line_1_trinary_label, 'tStartRefresh')  # time at next scr refresh
                    # add timestamp to datafile
                    thisExp.timestampOnFlip(win, 'line_1_trinary_label.started')
                    # update status
                    line_1_trinary_label.status = STARTED
                    line_1_trinary_label.setAutoDraw(True)
                
                # if line_1_trinary_label is active this frame...
                if line_1_trinary_label.status == STARTED:
                    # update params
                    pass
                
                # *line_2_trinary_label* updates
                
                # if line_2_trinary_label is starting this frame...
                if line_2_trinary_label.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                    # keep track of start time/frame for later
                    line_2_trinary_label.frameNStart = frameN  # exact frame index
                    line_2_trinary_label.tStart = t  # local t and not account for scr refresh
                    line_2_trinary_label.tStartRefresh = tThisFlipGlobal  # on global time
                    win.timeOnFlip(line_2_trinary_label, 'tStartRefresh')  # time at next scr refresh
                    # add timestamp to datafile
                    thisExp.timestampOnFlip(win, 'line_2_trinary_label.started')
                    # update status
                    line_2_trinary_label.status = STARTED
                    line_2_trinary_label.setAutoDraw(True)
                
                # if line_2_trinary_label is active this frame...
                if line_2_trinary_label.status == STARTED:
                    # update params
                    pass
                
                # *line_3_trinary_label* updates
                
                # if line_3_trinary_label is starting this frame...
                if line_3_trinary_label.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                    # keep track of start time/frame for later
                    line_3_trinary_label.frameNStart = frameN  # exact frame index
                    line_3_trinary_label.tStart = t  # local t and not account for scr refresh
                    line_3_trinary_label.tStartRefresh = tThisFlipGlobal  # on global time
                    win.timeOnFlip(line_3_trinary_label, 'tStartRefresh')  # time at next scr refresh
                    # add timestamp to datafile
                    thisExp.timestampOnFlip(win, 'line_3_trinary_label.started')
                    # update status
                    line_3_trinary_label.status = STARTED
                    line_3_trinary_label.setAutoDraw(True)
                
                # if line_3_trinary_label is active this frame...
                if line_3_trinary_label.status == STARTED:
                    # update params
                    pass
                
                # *transfer_trinary_prompt* updates
                
                # if transfer_trinary_prompt is starting this frame...
                if transfer_trinary_prompt.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                    # keep track of start time/frame for later
                    transfer_trinary_prompt.frameNStart = frameN  # exact frame index
                    transfer_trinary_prompt.tStart = t  # local t and not account for scr refresh
                    transfer_trinary_prompt.tStartRefresh = tThisFlipGlobal  # on global time
                    win.timeOnFlip(transfer_trinary_prompt, 'tStartRefresh')  # time at next scr refresh
                    # add timestamp to datafile
                    thisExp.timestampOnFlip(win, 'transfer_trinary_prompt.started')
                    # update status
                    transfer_trinary_prompt.status = STARTED
                    transfer_trinary_prompt.setAutoDraw(True)
                
                # if transfer_trinary_prompt is active this frame...
                if transfer_trinary_prompt.status == STARTED:
                    # update params
                    pass
                
                # check for quit (typically the Esc key)
                if defaultKeyboard.getKeys(keyList=["escape"]):
                    thisExp.status = FINISHED
                if thisExp.status == FINISHED or endExpNow:
                    endExperiment(thisExp, win=win)
                    return
                # pause experiment here if requested
                if thisExp.status == PAUSED:
                    pauseExperiment(
                        thisExp=thisExp, 
                        win=win, 
                        timers=[routineTimer], 
                        playbackComponents=[]
                    )
                    # skip the frame we paused on
                    continue
                
                # check if all components have finished
                if not continueRoutine:  # a component has requested a forced-end of Routine
                    transfer_trial_trinary.forceEnded = routineForceEnded = True
                    break
                continueRoutine = False  # will revert to True if at least one component still running
                for thisComponent in transfer_trial_trinary.components:
                    if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                        continueRoutine = True
                        break  # at least one component has not yet finished
                
                # refresh the screen
                if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
                    win.flip()
            
            # --- Ending Routine "transfer_trial_trinary" ---
            for thisComponent in transfer_trial_trinary.components:
                if hasattr(thisComponent, "setAutoDraw"):
                    thisComponent.setAutoDraw(False)
            # store stop times for transfer_trial_trinary
            transfer_trial_trinary.tStop = globalClock.getTime(format='float')
            transfer_trial_trinary.tStopRefresh = tThisFlipGlobal
            thisExp.addData('transfer_trial_trinary.stopped', transfer_trial_trinary.tStop)
            # check responses
            if key_resp_transfer_trinary.keys in ['', [], None]:  # No response was made
                key_resp_transfer_trinary.keys = None
            transfer_trinary_dummy.addData('key_resp_transfer_trinary.keys',key_resp_transfer_trinary.keys)
            if key_resp_transfer_trinary.keys != None:  # we had a response
                transfer_trinary_dummy.addData('key_resp_transfer_trinary.rt', key_resp_transfer_trinary.rt)
                transfer_trinary_dummy.addData('key_resp_transfer_trinary.duration', key_resp_transfer_trinary.duration)
            # Run 'End Routine' code from check_correct_trinary
            if(key_resp_transfer_trinary.keys[0] ==  key_correct_transfer_trinary_1):
                key_resp_transfer_trinary.corr=1
            elif (key_resp_transfer_trinary.keys[0] == key_correct_transfer_trinary_2):
                 key_resp_transfer_trinary.corr=1
            else:
                 key_resp_transfer_trinary.corr=0
            
            thisExp.addData('key_resp_transfer_trinary.corr',key_resp_transfer_trinary.corr)
            # the Routine "transfer_trial_trinary" was not non-slip safe, so reset the non-slip timer
            routineTimer.reset()
            
            # --- Prepare to start Routine "transfer_trinary_feedback" ---
            # create an object to store info about Routine transfer_trinary_feedback
            transfer_trinary_feedback = data.Routine(
                name='transfer_trinary_feedback',
                components=[transfer_trinary_feedback_text_display],
            )
            transfer_trinary_feedback.status = NOT_STARTED
            continueRoutine = True
            # update component parameters for each repeat
            # Run 'Begin Routine' code from transfer_trinary_text_code
            if key_resp_transfer_trinary.corr==1:
                transfer_trinary_feedback_dur=1;
                transfer_trinary_feedback_col="#008000"
                transfer_trinary_feedback_text="Correct!"
            elif key_resp_transfer_trinary.corr==0:
                transfer_trinary_feedback_dur=2;
                transfer_trinary_feedback_col="#FF0000"
                transfer_trinary_feedback_text="Incorrect."
            transfer_trinary_feedback_text_display.setColor(transfer_trinary_feedback_col, colorSpace='rgb')
            transfer_trinary_feedback_text_display.setPos((0,-150))
            transfer_trinary_feedback_text_display.setText(transfer_trinary_feedback_text
            
            )
            # store start times for transfer_trinary_feedback
            transfer_trinary_feedback.tStartRefresh = win.getFutureFlipTime(clock=globalClock)
            transfer_trinary_feedback.tStart = globalClock.getTime(format='float')
            transfer_trinary_feedback.status = STARTED
            thisExp.addData('transfer_trinary_feedback.started', transfer_trinary_feedback.tStart)
            transfer_trinary_feedback.maxDuration = None
            # keep track of which components have finished
            transfer_trinary_feedbackComponents = transfer_trinary_feedback.components
            for thisComponent in transfer_trinary_feedback.components:
                thisComponent.tStart = None
                thisComponent.tStop = None
                thisComponent.tStartRefresh = None
                thisComponent.tStopRefresh = None
                if hasattr(thisComponent, 'status'):
                    thisComponent.status = NOT_STARTED
            # reset timers
            t = 0
            _timeToFirstFrame = win.getFutureFlipTime(clock="now")
            frameN = -1
            
            # --- Run Routine "transfer_trinary_feedback" ---
            # if trial has changed, end Routine now
            if isinstance(transfer_trinary_dummy, data.TrialHandler2) and thisTransfer_trinary_dummy.thisN != transfer_trinary_dummy.thisTrial.thisN:
                continueRoutine = False
            transfer_trinary_feedback.forceEnded = routineForceEnded = not continueRoutine
            while continueRoutine:
                # get current time
                t = routineTimer.getTime()
                tThisFlip = win.getFutureFlipTime(clock=routineTimer)
                tThisFlipGlobal = win.getFutureFlipTime(clock=None)
                frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
                # update/draw components on each frame
                
                # *transfer_trinary_feedback_text_display* updates
                
                # if transfer_trinary_feedback_text_display is starting this frame...
                if transfer_trinary_feedback_text_display.status == NOT_STARTED and tThisFlip >= 0.5-frameTolerance:
                    # keep track of start time/frame for later
                    transfer_trinary_feedback_text_display.frameNStart = frameN  # exact frame index
                    transfer_trinary_feedback_text_display.tStart = t  # local t and not account for scr refresh
                    transfer_trinary_feedback_text_display.tStartRefresh = tThisFlipGlobal  # on global time
                    win.timeOnFlip(transfer_trinary_feedback_text_display, 'tStartRefresh')  # time at next scr refresh
                    # add timestamp to datafile
                    thisExp.timestampOnFlip(win, 'transfer_trinary_feedback_text_display.started')
                    # update status
                    transfer_trinary_feedback_text_display.status = STARTED
                    transfer_trinary_feedback_text_display.setAutoDraw(True)
                
                # if transfer_trinary_feedback_text_display is active this frame...
                if transfer_trinary_feedback_text_display.status == STARTED:
                    # update params
                    pass
                
                # if transfer_trinary_feedback_text_display is stopping this frame...
                if transfer_trinary_feedback_text_display.status == STARTED:
                    # is it time to stop? (based on global clock, using actual start)
                    if tThisFlipGlobal > transfer_trinary_feedback_text_display.tStartRefresh + transfer_trinary_feedback_dur-frameTolerance:
                        # keep track of stop time/frame for later
                        transfer_trinary_feedback_text_display.tStop = t  # not accounting for scr refresh
                        transfer_trinary_feedback_text_display.tStopRefresh = tThisFlipGlobal  # on global time
                        transfer_trinary_feedback_text_display.frameNStop = frameN  # exact frame index
                        # add timestamp to datafile
                        thisExp.timestampOnFlip(win, 'transfer_trinary_feedback_text_display.stopped')
                        # update status
                        transfer_trinary_feedback_text_display.status = FINISHED
                        transfer_trinary_feedback_text_display.setAutoDraw(False)
                
                # check for quit (typically the Esc key)
                if defaultKeyboard.getKeys(keyList=["escape"]):
                    thisExp.status = FINISHED
                if thisExp.status == FINISHED or endExpNow:
                    endExperiment(thisExp, win=win)
                    return
                # pause experiment here if requested
                if thisExp.status == PAUSED:
                    pauseExperiment(
                        thisExp=thisExp, 
                        win=win, 
                        timers=[routineTimer], 
                        playbackComponents=[]
                    )
                    # skip the frame we paused on
                    continue
                
                # check if all components have finished
                if not continueRoutine:  # a component has requested a forced-end of Routine
                    transfer_trinary_feedback.forceEnded = routineForceEnded = True
                    break
                continueRoutine = False  # will revert to True if at least one component still running
                for thisComponent in transfer_trinary_feedback.components:
                    if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                        continueRoutine = True
                        break  # at least one component has not yet finished
                
                # refresh the screen
                if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
                    win.flip()
            
            # --- Ending Routine "transfer_trinary_feedback" ---
            for thisComponent in transfer_trinary_feedback.components:
                if hasattr(thisComponent, "setAutoDraw"):
                    thisComponent.setAutoDraw(False)
            # store stop times for transfer_trinary_feedback
            transfer_trinary_feedback.tStop = globalClock.getTime(format='float')
            transfer_trinary_feedback.tStopRefresh = tThisFlipGlobal
            thisExp.addData('transfer_trinary_feedback.stopped', transfer_trinary_feedback.tStop)
            # the Routine "transfer_trinary_feedback" was not non-slip safe, so reset the non-slip timer
            routineTimer.reset()
            
            # --- Prepare to start Routine "blank" ---
            # create an object to store info about Routine blank
            blank = data.Routine(
                name='blank',
                components=[blank_200],
            )
            blank.status = NOT_STARTED
            continueRoutine = True
            # update component parameters for each repeat
            # store start times for blank
            blank.tStartRefresh = win.getFutureFlipTime(clock=globalClock)
            blank.tStart = globalClock.getTime(format='float')
            blank.status = STARTED
            thisExp.addData('blank.started', blank.tStart)
            blank.maxDuration = None
            # keep track of which components have finished
            blankComponents = blank.components
            for thisComponent in blank.components:
                thisComponent.tStart = None
                thisComponent.tStop = None
                thisComponent.tStartRefresh = None
                thisComponent.tStopRefresh = None
                if hasattr(thisComponent, 'status'):
                    thisComponent.status = NOT_STARTED
            # reset timers
            t = 0
            _timeToFirstFrame = win.getFutureFlipTime(clock="now")
            frameN = -1
            
            # --- Run Routine "blank" ---
            # if trial has changed, end Routine now
            if isinstance(transfer_trinary_dummy, data.TrialHandler2) and thisTransfer_trinary_dummy.thisN != transfer_trinary_dummy.thisTrial.thisN:
                continueRoutine = False
            blank.forceEnded = routineForceEnded = not continueRoutine
            while continueRoutine and routineTimer.getTime() < 0.2:
                # get current time
                t = routineTimer.getTime()
                tThisFlip = win.getFutureFlipTime(clock=routineTimer)
                tThisFlipGlobal = win.getFutureFlipTime(clock=None)
                frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
                # update/draw components on each frame
                
                # *blank_200* updates
                
                # if blank_200 is starting this frame...
                if blank_200.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                    # keep track of start time/frame for later
                    blank_200.frameNStart = frameN  # exact frame index
                    blank_200.tStart = t  # local t and not account for scr refresh
                    blank_200.tStartRefresh = tThisFlipGlobal  # on global time
                    win.timeOnFlip(blank_200, 'tStartRefresh')  # time at next scr refresh
                    # add timestamp to datafile
                    thisExp.timestampOnFlip(win, 'blank_200.started')
                    # update status
                    blank_200.status = STARTED
                    blank_200.setAutoDraw(True)
                
                # if blank_200 is active this frame...
                if blank_200.status == STARTED:
                    # update params
                    pass
                
                # if blank_200 is stopping this frame...
                if blank_200.status == STARTED:
                    # is it time to stop? (based on global clock, using actual start)
                    if tThisFlipGlobal > blank_200.tStartRefresh + 0.2-frameTolerance:
                        # keep track of stop time/frame for later
                        blank_200.tStop = t  # not accounting for scr refresh
                        blank_200.tStopRefresh = tThisFlipGlobal  # on global time
                        blank_200.frameNStop = frameN  # exact frame index
                        # add timestamp to datafile
                        thisExp.timestampOnFlip(win, 'blank_200.stopped')
                        # update status
                        blank_200.status = FINISHED
                        blank_200.setAutoDraw(False)
                
                # check for quit (typically the Esc key)
                if defaultKeyboard.getKeys(keyList=["escape"]):
                    thisExp.status = FINISHED
                if thisExp.status == FINISHED or endExpNow:
                    endExperiment(thisExp, win=win)
                    return
                # pause experiment here if requested
                if thisExp.status == PAUSED:
                    pauseExperiment(
                        thisExp=thisExp, 
                        win=win, 
                        timers=[routineTimer], 
                        playbackComponents=[]
                    )
                    # skip the frame we paused on
                    continue
                
                # check if all components have finished
                if not continueRoutine:  # a component has requested a forced-end of Routine
                    blank.forceEnded = routineForceEnded = True
                    break
                continueRoutine = False  # will revert to True if at least one component still running
                for thisComponent in blank.components:
                    if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                        continueRoutine = True
                        break  # at least one component has not yet finished
                
                # refresh the screen
                if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
                    win.flip()
            
            # --- Ending Routine "blank" ---
            for thisComponent in blank.components:
                if hasattr(thisComponent, "setAutoDraw"):
                    thisComponent.setAutoDraw(False)
            # store stop times for blank
            blank.tStop = globalClock.getTime(format='float')
            blank.tStopRefresh = tThisFlipGlobal
            thisExp.addData('blank.stopped', blank.tStop)
            # using non-slip timing so subtract the expected duration of this Routine (unless ended on request)
            if blank.maxDurationReached:
                routineTimer.addTime(-blank.maxDuration)
            elif blank.forceEnded:
                routineTimer.reset()
            else:
                routineTimer.addTime(-0.200000)
            thisExp.nextEntry()
            
        # completed do_trinary repeats of 'transfer_trinary_dummy'
        
        if thisSession is not None:
            # if running in a Session with a Liaison client, send data up to now
            thisSession.sendExperimentData()
        
        # set up handler to look after randomisation of conditions etc
        transfer_binary_dummy = data.TrialHandler2(
            name='transfer_binary_dummy',
            nReps=do_binary, 
            method='random', 
            extraInfo=expInfo, 
            originPath=-1, 
            trialList=[None], 
            seed=None, 
        )
        thisExp.addLoop(transfer_binary_dummy)  # add the loop to the experiment
        thisTransfer_binary_dummy = transfer_binary_dummy.trialList[0]  # so we can initialise stimuli with some values
        # abbreviate parameter names if possible (e.g. rgb = thisTransfer_binary_dummy.rgb)
        if thisTransfer_binary_dummy != None:
            for paramName in thisTransfer_binary_dummy:
                globals()[paramName] = thisTransfer_binary_dummy[paramName]
        if thisSession is not None:
            # if running in a Session with a Liaison client, send data up to now
            thisSession.sendExperimentData()
        
        for thisTransfer_binary_dummy in transfer_binary_dummy:
            currentLoop = transfer_binary_dummy
            thisExp.timestampOnFlip(win, 'thisRow.t', format=globalClock.format)
            if thisSession is not None:
                # if running in a Session with a Liaison client, send data up to now
                thisSession.sendExperimentData()
            # abbreviate parameter names if possible (e.g. rgb = thisTransfer_binary_dummy.rgb)
            if thisTransfer_binary_dummy != None:
                for paramName in thisTransfer_binary_dummy:
                    globals()[paramName] = thisTransfer_binary_dummy[paramName]
            
            # --- Prepare to start Routine "transfer_trial_binary" ---
            # create an object to store info about Routine transfer_trial_binary
            transfer_trial_binary = data.Routine(
                name='transfer_trial_binary',
                components=[line_1_binary, line_2_binary, line_1_label_binary, line_2_label_binary, transfer_binary_prompt, key_resp_transfer_binary],
            )
            transfer_trial_binary.status = NOT_STARTED
            continueRoutine = True
            # update component parameters for each repeat
            line_1_binary.setPos((-dist_btw/2,0))
            line_1_binary.setSize((line_width,stim_1_phy_shuffled))
            line_2_binary.setPos((dist_btw/2, 0))
            line_2_binary.setSize((line_width,stim_2_phy_shuffled))
            # create starting attributes for key_resp_transfer_binary
            key_resp_transfer_binary.keys = []
            key_resp_transfer_binary.rt = []
            _key_resp_transfer_binary_allKeys = []
            # store start times for transfer_trial_binary
            transfer_trial_binary.tStartRefresh = win.getFutureFlipTime(clock=globalClock)
            transfer_trial_binary.tStart = globalClock.getTime(format='float')
            transfer_trial_binary.status = STARTED
            thisExp.addData('transfer_trial_binary.started', transfer_trial_binary.tStart)
            transfer_trial_binary.maxDuration = None
            # keep track of which components have finished
            transfer_trial_binaryComponents = transfer_trial_binary.components
            for thisComponent in transfer_trial_binary.components:
                thisComponent.tStart = None
                thisComponent.tStop = None
                thisComponent.tStartRefresh = None
                thisComponent.tStopRefresh = None
                if hasattr(thisComponent, 'status'):
                    thisComponent.status = NOT_STARTED
            # reset timers
            t = 0
            _timeToFirstFrame = win.getFutureFlipTime(clock="now")
            frameN = -1
            
            # --- Run Routine "transfer_trial_binary" ---
            # if trial has changed, end Routine now
            if isinstance(transfer_binary_dummy, data.TrialHandler2) and thisTransfer_binary_dummy.thisN != transfer_binary_dummy.thisTrial.thisN:
                continueRoutine = False
            transfer_trial_binary.forceEnded = routineForceEnded = not continueRoutine
            while continueRoutine:
                # get current time
                t = routineTimer.getTime()
                tThisFlip = win.getFutureFlipTime(clock=routineTimer)
                tThisFlipGlobal = win.getFutureFlipTime(clock=None)
                frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
                # update/draw components on each frame
                
                # *line_1_binary* updates
                
                # if line_1_binary is starting this frame...
                if line_1_binary.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                    # keep track of start time/frame for later
                    line_1_binary.frameNStart = frameN  # exact frame index
                    line_1_binary.tStart = t  # local t and not account for scr refresh
                    line_1_binary.tStartRefresh = tThisFlipGlobal  # on global time
                    win.timeOnFlip(line_1_binary, 'tStartRefresh')  # time at next scr refresh
                    # add timestamp to datafile
                    thisExp.timestampOnFlip(win, 'line_1_binary.started')
                    # update status
                    line_1_binary.status = STARTED
                    line_1_binary.setAutoDraw(True)
                
                # if line_1_binary is active this frame...
                if line_1_binary.status == STARTED:
                    # update params
                    pass
                
                # *line_2_binary* updates
                
                # if line_2_binary is starting this frame...
                if line_2_binary.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                    # keep track of start time/frame for later
                    line_2_binary.frameNStart = frameN  # exact frame index
                    line_2_binary.tStart = t  # local t and not account for scr refresh
                    line_2_binary.tStartRefresh = tThisFlipGlobal  # on global time
                    win.timeOnFlip(line_2_binary, 'tStartRefresh')  # time at next scr refresh
                    # add timestamp to datafile
                    thisExp.timestampOnFlip(win, 'line_2_binary.started')
                    # update status
                    line_2_binary.status = STARTED
                    line_2_binary.setAutoDraw(True)
                
                # if line_2_binary is active this frame...
                if line_2_binary.status == STARTED:
                    # update params
                    pass
                
                # *line_1_label_binary* updates
                
                # if line_1_label_binary is starting this frame...
                if line_1_label_binary.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                    # keep track of start time/frame for later
                    line_1_label_binary.frameNStart = frameN  # exact frame index
                    line_1_label_binary.tStart = t  # local t and not account for scr refresh
                    line_1_label_binary.tStartRefresh = tThisFlipGlobal  # on global time
                    win.timeOnFlip(line_1_label_binary, 'tStartRefresh')  # time at next scr refresh
                    # add timestamp to datafile
                    thisExp.timestampOnFlip(win, 'line_1_label_binary.started')
                    # update status
                    line_1_label_binary.status = STARTED
                    line_1_label_binary.setAutoDraw(True)
                
                # if line_1_label_binary is active this frame...
                if line_1_label_binary.status == STARTED:
                    # update params
                    pass
                
                # *line_2_label_binary* updates
                
                # if line_2_label_binary is starting this frame...
                if line_2_label_binary.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                    # keep track of start time/frame for later
                    line_2_label_binary.frameNStart = frameN  # exact frame index
                    line_2_label_binary.tStart = t  # local t and not account for scr refresh
                    line_2_label_binary.tStartRefresh = tThisFlipGlobal  # on global time
                    win.timeOnFlip(line_2_label_binary, 'tStartRefresh')  # time at next scr refresh
                    # add timestamp to datafile
                    thisExp.timestampOnFlip(win, 'line_2_label_binary.started')
                    # update status
                    line_2_label_binary.status = STARTED
                    line_2_label_binary.setAutoDraw(True)
                
                # if line_2_label_binary is active this frame...
                if line_2_label_binary.status == STARTED:
                    # update params
                    pass
                
                # *transfer_binary_prompt* updates
                
                # if transfer_binary_prompt is starting this frame...
                if transfer_binary_prompt.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                    # keep track of start time/frame for later
                    transfer_binary_prompt.frameNStart = frameN  # exact frame index
                    transfer_binary_prompt.tStart = t  # local t and not account for scr refresh
                    transfer_binary_prompt.tStartRefresh = tThisFlipGlobal  # on global time
                    win.timeOnFlip(transfer_binary_prompt, 'tStartRefresh')  # time at next scr refresh
                    # add timestamp to datafile
                    thisExp.timestampOnFlip(win, 'transfer_binary_prompt.started')
                    # update status
                    transfer_binary_prompt.status = STARTED
                    transfer_binary_prompt.setAutoDraw(True)
                
                # if transfer_binary_prompt is active this frame...
                if transfer_binary_prompt.status == STARTED:
                    # update params
                    pass
                
                # *key_resp_transfer_binary* updates
                waitOnFlip = False
                
                # if key_resp_transfer_binary is starting this frame...
                if key_resp_transfer_binary.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                    # keep track of start time/frame for later
                    key_resp_transfer_binary.frameNStart = frameN  # exact frame index
                    key_resp_transfer_binary.tStart = t  # local t and not account for scr refresh
                    key_resp_transfer_binary.tStartRefresh = tThisFlipGlobal  # on global time
                    win.timeOnFlip(key_resp_transfer_binary, 'tStartRefresh')  # time at next scr refresh
                    # add timestamp to datafile
                    thisExp.timestampOnFlip(win, 'key_resp_transfer_binary.started')
                    # update status
                    key_resp_transfer_binary.status = STARTED
                    # keyboard checking is just starting
                    waitOnFlip = True
                    win.callOnFlip(key_resp_transfer_binary.clock.reset)  # t=0 on next screen flip
                    win.callOnFlip(key_resp_transfer_binary.clearEvents, eventType='keyboard')  # clear events on next screen flip
                if key_resp_transfer_binary.status == STARTED and not waitOnFlip:
                    theseKeys = key_resp_transfer_binary.getKeys(keyList=['j','k'], ignoreKeys=["escape"], waitRelease=False)
                    _key_resp_transfer_binary_allKeys.extend(theseKeys)
                    if len(_key_resp_transfer_binary_allKeys):
                        key_resp_transfer_binary.keys = _key_resp_transfer_binary_allKeys[-1].name  # just the last key pressed
                        key_resp_transfer_binary.rt = _key_resp_transfer_binary_allKeys[-1].rt
                        key_resp_transfer_binary.duration = _key_resp_transfer_binary_allKeys[-1].duration
                        # a response ends the routine
                        continueRoutine = False
                
                # check for quit (typically the Esc key)
                if defaultKeyboard.getKeys(keyList=["escape"]):
                    thisExp.status = FINISHED
                if thisExp.status == FINISHED or endExpNow:
                    endExperiment(thisExp, win=win)
                    return
                # pause experiment here if requested
                if thisExp.status == PAUSED:
                    pauseExperiment(
                        thisExp=thisExp, 
                        win=win, 
                        timers=[routineTimer], 
                        playbackComponents=[]
                    )
                    # skip the frame we paused on
                    continue
                
                # check if all components have finished
                if not continueRoutine:  # a component has requested a forced-end of Routine
                    transfer_trial_binary.forceEnded = routineForceEnded = True
                    break
                continueRoutine = False  # will revert to True if at least one component still running
                for thisComponent in transfer_trial_binary.components:
                    if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                        continueRoutine = True
                        break  # at least one component has not yet finished
                
                # refresh the screen
                if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
                    win.flip()
            
            # --- Ending Routine "transfer_trial_binary" ---
            for thisComponent in transfer_trial_binary.components:
                if hasattr(thisComponent, "setAutoDraw"):
                    thisComponent.setAutoDraw(False)
            # store stop times for transfer_trial_binary
            transfer_trial_binary.tStop = globalClock.getTime(format='float')
            transfer_trial_binary.tStopRefresh = tThisFlipGlobal
            thisExp.addData('transfer_trial_binary.stopped', transfer_trial_binary.tStop)
            # check responses
            if key_resp_transfer_binary.keys in ['', [], None]:  # No response was made
                key_resp_transfer_binary.keys = None
            transfer_binary_dummy.addData('key_resp_transfer_binary.keys',key_resp_transfer_binary.keys)
            if key_resp_transfer_binary.keys != None:  # we had a response
                transfer_binary_dummy.addData('key_resp_transfer_binary.rt', key_resp_transfer_binary.rt)
                transfer_binary_dummy.addData('key_resp_transfer_binary.duration', key_resp_transfer_binary.duration)
            # Run 'End Routine' code from check_correct_binary
            if(key_resp_transfer_binary.keys[0] ==  key_correct_transfer_binary_1):
                key_resp_transfer_binary.corr=1
            elif (key_resp_transfer_binary.keys[0] == key_correct_transfer_binary_2):
                key_resp_transfer_binary.corr=1
            else:
                key_resp_transfer_binary.corr=0
            
            thisExp.addData('key_resp_transfer_binary.corr',key_resp_transfer_binary.corr)
            
            # the Routine "transfer_trial_binary" was not non-slip safe, so reset the non-slip timer
            routineTimer.reset()
            
            # --- Prepare to start Routine "transfer_binary_feedback" ---
            # create an object to store info about Routine transfer_binary_feedback
            transfer_binary_feedback = data.Routine(
                name='transfer_binary_feedback',
                components=[transfer_binary_feedback_text_display],
            )
            transfer_binary_feedback.status = NOT_STARTED
            continueRoutine = True
            # update component parameters for each repeat
            # Run 'Begin Routine' code from transfer_binary_feedback_text_code
            if key_resp_transfer_binary.corr==1:
                transfer_binary_feedback_dur=1;
                transfer_binary_feedback_col="#008000"
                transfer_binary_feedback_text="Correct!"
            elif key_resp_transfer_binary.corr==0:
                transfer_binary_feedback_dur=2;
                transfer_binary_feedback_col="#FF0000"
                transfer_binary_feedback_text="Incorrect."
            transfer_binary_feedback_text_display.setColor(transfer_binary_feedback_col, colorSpace='rgb')
            transfer_binary_feedback_text_display.setText(transfer_binary_feedback_text
            )
            # store start times for transfer_binary_feedback
            transfer_binary_feedback.tStartRefresh = win.getFutureFlipTime(clock=globalClock)
            transfer_binary_feedback.tStart = globalClock.getTime(format='float')
            transfer_binary_feedback.status = STARTED
            thisExp.addData('transfer_binary_feedback.started', transfer_binary_feedback.tStart)
            transfer_binary_feedback.maxDuration = None
            # keep track of which components have finished
            transfer_binary_feedbackComponents = transfer_binary_feedback.components
            for thisComponent in transfer_binary_feedback.components:
                thisComponent.tStart = None
                thisComponent.tStop = None
                thisComponent.tStartRefresh = None
                thisComponent.tStopRefresh = None
                if hasattr(thisComponent, 'status'):
                    thisComponent.status = NOT_STARTED
            # reset timers
            t = 0
            _timeToFirstFrame = win.getFutureFlipTime(clock="now")
            frameN = -1
            
            # --- Run Routine "transfer_binary_feedback" ---
            # if trial has changed, end Routine now
            if isinstance(transfer_binary_dummy, data.TrialHandler2) and thisTransfer_binary_dummy.thisN != transfer_binary_dummy.thisTrial.thisN:
                continueRoutine = False
            transfer_binary_feedback.forceEnded = routineForceEnded = not continueRoutine
            while continueRoutine:
                # get current time
                t = routineTimer.getTime()
                tThisFlip = win.getFutureFlipTime(clock=routineTimer)
                tThisFlipGlobal = win.getFutureFlipTime(clock=None)
                frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
                # update/draw components on each frame
                
                # *transfer_binary_feedback_text_display* updates
                
                # if transfer_binary_feedback_text_display is starting this frame...
                if transfer_binary_feedback_text_display.status == NOT_STARTED and tThisFlip >= 0.5-frameTolerance:
                    # keep track of start time/frame for later
                    transfer_binary_feedback_text_display.frameNStart = frameN  # exact frame index
                    transfer_binary_feedback_text_display.tStart = t  # local t and not account for scr refresh
                    transfer_binary_feedback_text_display.tStartRefresh = tThisFlipGlobal  # on global time
                    win.timeOnFlip(transfer_binary_feedback_text_display, 'tStartRefresh')  # time at next scr refresh
                    # add timestamp to datafile
                    thisExp.timestampOnFlip(win, 'transfer_binary_feedback_text_display.started')
                    # update status
                    transfer_binary_feedback_text_display.status = STARTED
                    transfer_binary_feedback_text_display.setAutoDraw(True)
                
                # if transfer_binary_feedback_text_display is active this frame...
                if transfer_binary_feedback_text_display.status == STARTED:
                    # update params
                    pass
                
                # if transfer_binary_feedback_text_display is stopping this frame...
                if transfer_binary_feedback_text_display.status == STARTED:
                    # is it time to stop? (based on global clock, using actual start)
                    if tThisFlipGlobal > transfer_binary_feedback_text_display.tStartRefresh + transfer_binary_feedback_dur-frameTolerance:
                        # keep track of stop time/frame for later
                        transfer_binary_feedback_text_display.tStop = t  # not accounting for scr refresh
                        transfer_binary_feedback_text_display.tStopRefresh = tThisFlipGlobal  # on global time
                        transfer_binary_feedback_text_display.frameNStop = frameN  # exact frame index
                        # add timestamp to datafile
                        thisExp.timestampOnFlip(win, 'transfer_binary_feedback_text_display.stopped')
                        # update status
                        transfer_binary_feedback_text_display.status = FINISHED
                        transfer_binary_feedback_text_display.setAutoDraw(False)
                
                # check for quit (typically the Esc key)
                if defaultKeyboard.getKeys(keyList=["escape"]):
                    thisExp.status = FINISHED
                if thisExp.status == FINISHED or endExpNow:
                    endExperiment(thisExp, win=win)
                    return
                # pause experiment here if requested
                if thisExp.status == PAUSED:
                    pauseExperiment(
                        thisExp=thisExp, 
                        win=win, 
                        timers=[routineTimer], 
                        playbackComponents=[]
                    )
                    # skip the frame we paused on
                    continue
                
                # check if all components have finished
                if not continueRoutine:  # a component has requested a forced-end of Routine
                    transfer_binary_feedback.forceEnded = routineForceEnded = True
                    break
                continueRoutine = False  # will revert to True if at least one component still running
                for thisComponent in transfer_binary_feedback.components:
                    if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                        continueRoutine = True
                        break  # at least one component has not yet finished
                
                # refresh the screen
                if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
                    win.flip()
            
            # --- Ending Routine "transfer_binary_feedback" ---
            for thisComponent in transfer_binary_feedback.components:
                if hasattr(thisComponent, "setAutoDraw"):
                    thisComponent.setAutoDraw(False)
            # store stop times for transfer_binary_feedback
            transfer_binary_feedback.tStop = globalClock.getTime(format='float')
            transfer_binary_feedback.tStopRefresh = tThisFlipGlobal
            thisExp.addData('transfer_binary_feedback.stopped', transfer_binary_feedback.tStop)
            # the Routine "transfer_binary_feedback" was not non-slip safe, so reset the non-slip timer
            routineTimer.reset()
            
            # --- Prepare to start Routine "blank" ---
            # create an object to store info about Routine blank
            blank = data.Routine(
                name='blank',
                components=[blank_200],
            )
            blank.status = NOT_STARTED
            continueRoutine = True
            # update component parameters for each repeat
            # store start times for blank
            blank.tStartRefresh = win.getFutureFlipTime(clock=globalClock)
            blank.tStart = globalClock.getTime(format='float')
            blank.status = STARTED
            thisExp.addData('blank.started', blank.tStart)
            blank.maxDuration = None
            # keep track of which components have finished
            blankComponents = blank.components
            for thisComponent in blank.components:
                thisComponent.tStart = None
                thisComponent.tStop = None
                thisComponent.tStartRefresh = None
                thisComponent.tStopRefresh = None
                if hasattr(thisComponent, 'status'):
                    thisComponent.status = NOT_STARTED
            # reset timers
            t = 0
            _timeToFirstFrame = win.getFutureFlipTime(clock="now")
            frameN = -1
            
            # --- Run Routine "blank" ---
            # if trial has changed, end Routine now
            if isinstance(transfer_binary_dummy, data.TrialHandler2) and thisTransfer_binary_dummy.thisN != transfer_binary_dummy.thisTrial.thisN:
                continueRoutine = False
            blank.forceEnded = routineForceEnded = not continueRoutine
            while continueRoutine and routineTimer.getTime() < 0.2:
                # get current time
                t = routineTimer.getTime()
                tThisFlip = win.getFutureFlipTime(clock=routineTimer)
                tThisFlipGlobal = win.getFutureFlipTime(clock=None)
                frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
                # update/draw components on each frame
                
                # *blank_200* updates
                
                # if blank_200 is starting this frame...
                if blank_200.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                    # keep track of start time/frame for later
                    blank_200.frameNStart = frameN  # exact frame index
                    blank_200.tStart = t  # local t and not account for scr refresh
                    blank_200.tStartRefresh = tThisFlipGlobal  # on global time
                    win.timeOnFlip(blank_200, 'tStartRefresh')  # time at next scr refresh
                    # add timestamp to datafile
                    thisExp.timestampOnFlip(win, 'blank_200.started')
                    # update status
                    blank_200.status = STARTED
                    blank_200.setAutoDraw(True)
                
                # if blank_200 is active this frame...
                if blank_200.status == STARTED:
                    # update params
                    pass
                
                # if blank_200 is stopping this frame...
                if blank_200.status == STARTED:
                    # is it time to stop? (based on global clock, using actual start)
                    if tThisFlipGlobal > blank_200.tStartRefresh + 0.2-frameTolerance:
                        # keep track of stop time/frame for later
                        blank_200.tStop = t  # not accounting for scr refresh
                        blank_200.tStopRefresh = tThisFlipGlobal  # on global time
                        blank_200.frameNStop = frameN  # exact frame index
                        # add timestamp to datafile
                        thisExp.timestampOnFlip(win, 'blank_200.stopped')
                        # update status
                        blank_200.status = FINISHED
                        blank_200.setAutoDraw(False)
                
                # check for quit (typically the Esc key)
                if defaultKeyboard.getKeys(keyList=["escape"]):
                    thisExp.status = FINISHED
                if thisExp.status == FINISHED or endExpNow:
                    endExperiment(thisExp, win=win)
                    return
                # pause experiment here if requested
                if thisExp.status == PAUSED:
                    pauseExperiment(
                        thisExp=thisExp, 
                        win=win, 
                        timers=[routineTimer], 
                        playbackComponents=[]
                    )
                    # skip the frame we paused on
                    continue
                
                # check if all components have finished
                if not continueRoutine:  # a component has requested a forced-end of Routine
                    blank.forceEnded = routineForceEnded = True
                    break
                continueRoutine = False  # will revert to True if at least one component still running
                for thisComponent in blank.components:
                    if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                        continueRoutine = True
                        break  # at least one component has not yet finished
                
                # refresh the screen
                if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
                    win.flip()
            
            # --- Ending Routine "blank" ---
            for thisComponent in blank.components:
                if hasattr(thisComponent, "setAutoDraw"):
                    thisComponent.setAutoDraw(False)
            # store stop times for blank
            blank.tStop = globalClock.getTime(format='float')
            blank.tStopRefresh = tThisFlipGlobal
            thisExp.addData('blank.stopped', blank.tStop)
            # using non-slip timing so subtract the expected duration of this Routine (unless ended on request)
            if blank.maxDurationReached:
                routineTimer.addTime(-blank.maxDuration)
            elif blank.forceEnded:
                routineTimer.reset()
            else:
                routineTimer.addTime(-0.200000)
            thisExp.nextEntry()
            
        # completed do_binary repeats of 'transfer_binary_dummy'
        
        if thisSession is not None:
            # if running in a Session with a Liaison client, send data up to now
            thisSession.sendExperimentData()
        thisExp.nextEntry()
        
    # completed 3.0 repeats of 'transfer_trials'
    
    if thisSession is not None:
        # if running in a Session with a Liaison client, send data up to now
        thisSession.sendExperimentData()
    
    # --- Prepare to start Routine "exp_finished" ---
    # create an object to store info about Routine exp_finished
    exp_finished = data.Routine(
        name='exp_finished',
        components=[exp_finished_txt1, exp_finished_txt2, exp_finished_txt3, cont_to_debrief],
    )
    exp_finished.status = NOT_STARTED
    continueRoutine = True
    # update component parameters for each repeat
    # create starting attributes for cont_to_debrief
    cont_to_debrief.keys = []
    cont_to_debrief.rt = []
    _cont_to_debrief_allKeys = []
    # store start times for exp_finished
    exp_finished.tStartRefresh = win.getFutureFlipTime(clock=globalClock)
    exp_finished.tStart = globalClock.getTime(format='float')
    exp_finished.status = STARTED
    thisExp.addData('exp_finished.started', exp_finished.tStart)
    exp_finished.maxDuration = None
    # keep track of which components have finished
    exp_finishedComponents = exp_finished.components
    for thisComponent in exp_finished.components:
        thisComponent.tStart = None
        thisComponent.tStop = None
        thisComponent.tStartRefresh = None
        thisComponent.tStopRefresh = None
        if hasattr(thisComponent, 'status'):
            thisComponent.status = NOT_STARTED
    # reset timers
    t = 0
    _timeToFirstFrame = win.getFutureFlipTime(clock="now")
    frameN = -1
    
    # --- Run Routine "exp_finished" ---
    exp_finished.forceEnded = routineForceEnded = not continueRoutine
    while continueRoutine:
        # get current time
        t = routineTimer.getTime()
        tThisFlip = win.getFutureFlipTime(clock=routineTimer)
        tThisFlipGlobal = win.getFutureFlipTime(clock=None)
        frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
        # update/draw components on each frame
        
        # *exp_finished_txt1* updates
        
        # if exp_finished_txt1 is starting this frame...
        if exp_finished_txt1.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            exp_finished_txt1.frameNStart = frameN  # exact frame index
            exp_finished_txt1.tStart = t  # local t and not account for scr refresh
            exp_finished_txt1.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(exp_finished_txt1, 'tStartRefresh')  # time at next scr refresh
            # add timestamp to datafile
            thisExp.timestampOnFlip(win, 'exp_finished_txt1.started')
            # update status
            exp_finished_txt1.status = STARTED
            exp_finished_txt1.setAutoDraw(True)
        
        # if exp_finished_txt1 is active this frame...
        if exp_finished_txt1.status == STARTED:
            # update params
            pass
        
        # *exp_finished_txt2* updates
        
        # if exp_finished_txt2 is starting this frame...
        if exp_finished_txt2.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            exp_finished_txt2.frameNStart = frameN  # exact frame index
            exp_finished_txt2.tStart = t  # local t and not account for scr refresh
            exp_finished_txt2.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(exp_finished_txt2, 'tStartRefresh')  # time at next scr refresh
            # add timestamp to datafile
            thisExp.timestampOnFlip(win, 'exp_finished_txt2.started')
            # update status
            exp_finished_txt2.status = STARTED
            exp_finished_txt2.setAutoDraw(True)
        
        # if exp_finished_txt2 is active this frame...
        if exp_finished_txt2.status == STARTED:
            # update params
            pass
        
        # *exp_finished_txt3* updates
        
        # if exp_finished_txt3 is starting this frame...
        if exp_finished_txt3.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            exp_finished_txt3.frameNStart = frameN  # exact frame index
            exp_finished_txt3.tStart = t  # local t and not account for scr refresh
            exp_finished_txt3.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(exp_finished_txt3, 'tStartRefresh')  # time at next scr refresh
            # add timestamp to datafile
            thisExp.timestampOnFlip(win, 'exp_finished_txt3.started')
            # update status
            exp_finished_txt3.status = STARTED
            exp_finished_txt3.setAutoDraw(True)
        
        # if exp_finished_txt3 is active this frame...
        if exp_finished_txt3.status == STARTED:
            # update params
            pass
        
        # *cont_to_debrief* updates
        waitOnFlip = False
        
        # if cont_to_debrief is starting this frame...
        if cont_to_debrief.status == NOT_STARTED and tThisFlip >= 1.5-frameTolerance:
            # keep track of start time/frame for later
            cont_to_debrief.frameNStart = frameN  # exact frame index
            cont_to_debrief.tStart = t  # local t and not account for scr refresh
            cont_to_debrief.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(cont_to_debrief, 'tStartRefresh')  # time at next scr refresh
            # add timestamp to datafile
            thisExp.timestampOnFlip(win, 'cont_to_debrief.started')
            # update status
            cont_to_debrief.status = STARTED
            # keyboard checking is just starting
            waitOnFlip = True
            win.callOnFlip(cont_to_debrief.clock.reset)  # t=0 on next screen flip
            win.callOnFlip(cont_to_debrief.clearEvents, eventType='keyboard')  # clear events on next screen flip
        if cont_to_debrief.status == STARTED and not waitOnFlip:
            theseKeys = cont_to_debrief.getKeys(keyList=['space'], ignoreKeys=["escape"], waitRelease=False)
            _cont_to_debrief_allKeys.extend(theseKeys)
            if len(_cont_to_debrief_allKeys):
                cont_to_debrief.keys = _cont_to_debrief_allKeys[-1].name  # just the last key pressed
                cont_to_debrief.rt = _cont_to_debrief_allKeys[-1].rt
                cont_to_debrief.duration = _cont_to_debrief_allKeys[-1].duration
                # a response ends the routine
                continueRoutine = False
        
        # check for quit (typically the Esc key)
        if defaultKeyboard.getKeys(keyList=["escape"]):
            thisExp.status = FINISHED
        if thisExp.status == FINISHED or endExpNow:
            endExperiment(thisExp, win=win)
            return
        # pause experiment here if requested
        if thisExp.status == PAUSED:
            pauseExperiment(
                thisExp=thisExp, 
                win=win, 
                timers=[routineTimer], 
                playbackComponents=[]
            )
            # skip the frame we paused on
            continue
        
        # check if all components have finished
        if not continueRoutine:  # a component has requested a forced-end of Routine
            exp_finished.forceEnded = routineForceEnded = True
            break
        continueRoutine = False  # will revert to True if at least one component still running
        for thisComponent in exp_finished.components:
            if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                continueRoutine = True
                break  # at least one component has not yet finished
        
        # refresh the screen
        if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
            win.flip()
    
    # --- Ending Routine "exp_finished" ---
    for thisComponent in exp_finished.components:
        if hasattr(thisComponent, "setAutoDraw"):
            thisComponent.setAutoDraw(False)
    # store stop times for exp_finished
    exp_finished.tStop = globalClock.getTime(format='float')
    exp_finished.tStopRefresh = tThisFlipGlobal
    thisExp.addData('exp_finished.stopped', exp_finished.tStop)
    # check responses
    if cont_to_debrief.keys in ['', [], None]:  # No response was made
        cont_to_debrief.keys = None
    thisExp.addData('cont_to_debrief.keys',cont_to_debrief.keys)
    if cont_to_debrief.keys != None:  # we had a response
        thisExp.addData('cont_to_debrief.rt', cont_to_debrief.rt)
        thisExp.addData('cont_to_debrief.duration', cont_to_debrief.duration)
    thisExp.nextEntry()
    # the Routine "exp_finished" was not non-slip safe, so reset the non-slip timer
    routineTimer.reset()
    
    # --- Prepare to start Routine "debriefing" ---
    # create an object to store info about Routine debriefing
    debriefing = data.Routine(
        name='debriefing',
        components=[debrief_form, end_debrief],
    )
    debriefing.status = NOT_STARTED
    continueRoutine = True
    # update component parameters for each repeat
    # create starting attributes for end_debrief
    end_debrief.keys = []
    end_debrief.rt = []
    _end_debrief_allKeys = []
    # store start times for debriefing
    debriefing.tStartRefresh = win.getFutureFlipTime(clock=globalClock)
    debriefing.tStart = globalClock.getTime(format='float')
    debriefing.status = STARTED
    thisExp.addData('debriefing.started', debriefing.tStart)
    debriefing.maxDuration = None
    # keep track of which components have finished
    debriefingComponents = debriefing.components
    for thisComponent in debriefing.components:
        thisComponent.tStart = None
        thisComponent.tStop = None
        thisComponent.tStartRefresh = None
        thisComponent.tStopRefresh = None
        if hasattr(thisComponent, 'status'):
            thisComponent.status = NOT_STARTED
    # reset timers
    t = 0
    _timeToFirstFrame = win.getFutureFlipTime(clock="now")
    frameN = -1
    
    # --- Run Routine "debriefing" ---
    debriefing.forceEnded = routineForceEnded = not continueRoutine
    while continueRoutine:
        # get current time
        t = routineTimer.getTime()
        tThisFlip = win.getFutureFlipTime(clock=routineTimer)
        tThisFlipGlobal = win.getFutureFlipTime(clock=None)
        frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
        # update/draw components on each frame
        
        # *debrief_form* updates
        
        # if debrief_form is starting this frame...
        if debrief_form.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            debrief_form.frameNStart = frameN  # exact frame index
            debrief_form.tStart = t  # local t and not account for scr refresh
            debrief_form.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(debrief_form, 'tStartRefresh')  # time at next scr refresh
            # add timestamp to datafile
            thisExp.timestampOnFlip(win, 'debrief_form.started')
            # update status
            debrief_form.status = STARTED
            debrief_form.setAutoDraw(True)
        
        # if debrief_form is active this frame...
        if debrief_form.status == STARTED:
            # update params
            pass
        
        # *end_debrief* updates
        waitOnFlip = False
        
        # if end_debrief is starting this frame...
        if end_debrief.status == NOT_STARTED and tThisFlip >= 4.0-frameTolerance:
            # keep track of start time/frame for later
            end_debrief.frameNStart = frameN  # exact frame index
            end_debrief.tStart = t  # local t and not account for scr refresh
            end_debrief.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(end_debrief, 'tStartRefresh')  # time at next scr refresh
            # add timestamp to datafile
            thisExp.timestampOnFlip(win, 'end_debrief.started')
            # update status
            end_debrief.status = STARTED
            # keyboard checking is just starting
            waitOnFlip = True
            win.callOnFlip(end_debrief.clock.reset)  # t=0 on next screen flip
            win.callOnFlip(end_debrief.clearEvents, eventType='keyboard')  # clear events on next screen flip
        if end_debrief.status == STARTED and not waitOnFlip:
            theseKeys = end_debrief.getKeys(keyList=['space'], ignoreKeys=["escape"], waitRelease=False)
            _end_debrief_allKeys.extend(theseKeys)
            if len(_end_debrief_allKeys):
                end_debrief.keys = _end_debrief_allKeys[-1].name  # just the last key pressed
                end_debrief.rt = _end_debrief_allKeys[-1].rt
                end_debrief.duration = _end_debrief_allKeys[-1].duration
                # a response ends the routine
                continueRoutine = False
        
        # check for quit (typically the Esc key)
        if defaultKeyboard.getKeys(keyList=["escape"]):
            thisExp.status = FINISHED
        if thisExp.status == FINISHED or endExpNow:
            endExperiment(thisExp, win=win)
            return
        # pause experiment here if requested
        if thisExp.status == PAUSED:
            pauseExperiment(
                thisExp=thisExp, 
                win=win, 
                timers=[routineTimer], 
                playbackComponents=[]
            )
            # skip the frame we paused on
            continue
        
        # check if all components have finished
        if not continueRoutine:  # a component has requested a forced-end of Routine
            debriefing.forceEnded = routineForceEnded = True
            break
        continueRoutine = False  # will revert to True if at least one component still running
        for thisComponent in debriefing.components:
            if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                continueRoutine = True
                break  # at least one component has not yet finished
        
        # refresh the screen
        if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
            win.flip()
    
    # --- Ending Routine "debriefing" ---
    for thisComponent in debriefing.components:
        if hasattr(thisComponent, "setAutoDraw"):
            thisComponent.setAutoDraw(False)
    # store stop times for debriefing
    debriefing.tStop = globalClock.getTime(format='float')
    debriefing.tStopRefresh = tThisFlipGlobal
    thisExp.addData('debriefing.stopped', debriefing.tStop)
    # check responses
    if end_debrief.keys in ['', [], None]:  # No response was made
        end_debrief.keys = None
    thisExp.addData('end_debrief.keys',end_debrief.keys)
    if end_debrief.keys != None:  # we had a response
        thisExp.addData('end_debrief.rt', end_debrief.rt)
        thisExp.addData('end_debrief.duration', end_debrief.duration)
    thisExp.nextEntry()
    # the Routine "debriefing" was not non-slip safe, so reset the non-slip timer
    routineTimer.reset()
    
    # mark experiment as finished
    endExperiment(thisExp, win=win)


def saveData(thisExp):
    """
    Save data from this experiment
    
    Parameters
    ==========
    thisExp : psychopy.data.ExperimentHandler
        Handler object for this experiment, contains the data to save and information about 
        where to save it to.
    """
    filename = thisExp.dataFileName
    # these shouldn't be strictly necessary (should auto-save)
    thisExp.saveAsWideText(filename + '.csv', delim='auto')
    thisExp.saveAsPickle(filename)


def endExperiment(thisExp, win=None):
    """
    End this experiment, performing final shut down operations.
    
    This function does NOT close the window or end the Python process - use `quit` for this.
    
    Parameters
    ==========
    thisExp : psychopy.data.ExperimentHandler
        Handler object for this experiment, contains the data to save and information about 
        where to save it to.
    win : psychopy.visual.Window
        Window for this experiment.
    """
    if win is not None:
        # remove autodraw from all current components
        win.clearAutoDraw()
        # Flip one final time so any remaining win.callOnFlip() 
        # and win.timeOnFlip() tasks get executed
        win.flip()
    # return console logger level to WARNING
    logging.console.setLevel(logging.WARNING)
    # mark experiment handler as finished
    thisExp.status = FINISHED
    logging.flush()


def quit(thisExp, win=None, thisSession=None):
    """
    Fully quit, closing the window and ending the Python process.
    
    Parameters
    ==========
    win : psychopy.visual.Window
        Window to close.
    thisSession : psychopy.session.Session or None
        Handle of the Session object this experiment is being run from, if any.
    """
    thisExp.abort()  # or data files will save again on exit
    # make sure everything is closed down
    if win is not None:
        # Flip one final time so any remaining win.callOnFlip() 
        # and win.timeOnFlip() tasks get executed before quitting
        win.flip()
        win.close()
    logging.flush()
    if thisSession is not None:
        thisSession.stop()
    # terminate Python process
    core.quit()


# if running this experiment as a script...
if __name__ == '__main__':
    # call all functions in order
    expInfo = showExpInfoDlg(expInfo=expInfo)
    thisExp = setupData(expInfo=expInfo)
    logFile = setupLogging(filename=thisExp.dataFileName)
    win = setupWindow(expInfo=expInfo)
    setupDevices(expInfo=expInfo, thisExp=thisExp, win=win)
    run(
        expInfo=expInfo, 
        thisExp=thisExp, 
        win=win,
        globalClock='float'
    )
    saveData(thisExp=thisExp)
    quit(thisExp=thisExp, win=win)
