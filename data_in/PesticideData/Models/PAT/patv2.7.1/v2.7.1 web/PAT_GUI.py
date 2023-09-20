# -*- coding: utf-8 -*-
"""
Created on Mon Jan 24 13:52:36 2022

@author: GDYKES
"""

import tkinter as tk
from tkinter.ttk import *
import tkinter.filedialog
import os
import patv2_7_1

# In[]:
### Build the GUI
window = tk.Tk()
window.title('PAT: Plant Assessment Tool (v2.7.1)')
window.geometry("1150x525")
window.resizable(1,1)

# PAT Run Information
frame_run_info = tk.LabelFrame(master = window, text = "PAT Run Information")

### Terrestrial
def isTerrestrial():
    if terrestrial.get() == True:
        check_saveteroutput['state'] = 'normal'
    elif terrestrial.get() == False:
        check_saveteroutput['state'] = 'disabled'
        saveteroutput = False

terrestrial = tk.BooleanVar()
check_terrestrial = tk.Checkbutton(master = frame_run_info, command = isTerrestrial, text = "Terrestrial (TPEZ)", variable = terrestrial, onvalue = True, offvalue = False)
check_terrestrial.grid(column = 1, row = 1, pady=5, padx=5, sticky="NW")

### Aquatic
aquatic = tk.BooleanVar()
check_aquatic = tk.Checkbutton(master = frame_run_info, text = "Aquatic", variable = aquatic, onvalue = True, offvalue = False)
check_aquatic.grid(column = 1, row = 0, pady=5, padx=5, sticky="NW")

### Semi-aquatic
def isSemiaquatic():
    if semiaquatic.get() == True:
        check_savesemioutput['state'] = 'normal'
    elif semiaquatic.get() == False:
        check_savesemioutput['state'] = 'disable'
        savesemioutput = False
        
semiaquatic = tk.BooleanVar()
check_semiaquatic = tk.Checkbutton(master = frame_run_info, command = isSemiaquatic, text = "Wetland (WPEZ)", variable = semiaquatic, onvalue = True, offvalue = False)
check_semiaquatic.grid(column = 1, row = 2, pady=5, padx=5, sticky="NW")

## Save output?
# Terrestrial output
save_terr_gui = tk.BooleanVar()
check_saveteroutput = tk.Checkbutton(master = frame_run_info, text = "Save terrestrial output?", variable = save_terr_gui, onvalue = True, offvalue = False, state = "disable")
check_saveteroutput.grid(column = 2, row = 1, pady=5, padx=5, sticky="NW")

# Semi-aquatic output
save_saq_gui = tk.BooleanVar()
check_savesemioutput = tk.Checkbutton(master = frame_run_info, text = "Save wetland output?", variable = save_saq_gui, onvalue = True, offvalue = False, state = "disable")
check_savesemioutput.grid(column = 2, row = 2, pady=5, padx=5, sticky="NW")

## ESA?
esa_gui = tk.BooleanVar()
check_esa = tk.Checkbutton(master = frame_run_info, text = "ESA (1-in-15 yr)", variable = esa_gui, onvalue = True, offvalue = False, state = 'disable')
check_esa.grid(column = 0, row = 0, pady=5, padx=5, sticky="NW")

## Make figures?
make_figs_gui = tk.BooleanVar()
check_make_figs = tk.Checkbutton(master = frame_run_info, text = "Make figures", variable = make_figs_gui, onvalue = True, offvalue = False, state = "disable")
check_make_figs.grid(column = 0, row = 2, pady=5, padx=5, sticky="NW")

# FIFRA?
fifra_run = tk.BooleanVar()
check_fifra_run = tk.Checkbutton(master = frame_run_info, text ="ESA/FIFRA (1-in-10 yr)", variable = fifra_run, onvalue = True, offvalue = False)
check_fifra_run.grid(column = 0, row = 1, pady=5, padx=5, sticky="NW")

# In[]:
# Input directory
frame_input_dir = tk.LabelFrame(master = window, text = "Inputs")

def openfile_input():
    input_dir = tk.filedialog.askdirectory(title="Open file")
    entry_input_dir.delete(0, tk.END)
    entry_input_dir.insert(tk.END, input_dir) # add this

### Button
button_input_dir = tk.Button(master = frame_input_dir, width=30, text='Browse for input directory', command=openfile_input).grid(column = 0, row = 0, stick = "NSEW", pady=5, padx=5)

### Entry
entry_input_dir = tk.Entry(master = frame_input_dir, width = 50)
entry_input_dir.grid(column = 1, row = 0, columnspan=1, pady=5, padx=5, stick = "NSEW")

# In[]:
# Output directory
### Label
label_output_dir = tk.Label(master = frame_input_dir, text = 'Output directory name').grid(column = 0, row = 1, pady=5, padx=5, sticky = "NSEW")

### Entry
entry_output_dir = tk.Entry(master = frame_input_dir, width = 50)
entry_output_dir.grid(column = 1, row = 1, columnspan=1, pady=5, padx=5, sticky = "NSEW")

# In[]:
# PAT batch run
frame_batch_run = tk.LabelFrame(master = window, text = "PAT Batch Run")
frame_batch_run_sub1 = tk.Frame(master = frame_batch_run)
frame_batch_run_sub2 = tk.Frame(master = frame_batch_run)

def isChecked_PAT_b():
    if batch_run_gui.get() == True:
        for child in frame_batch_run_sub2.winfo_children():
            child.configure(state='normal')
    elif batch_run_gui.get() == False:
        for child in frame_batch_run_sub2.winfo_children():
            child.configure(state='disable')

batch_run_gui = tk.BooleanVar()

check_batch_run = tk.Checkbutton(master = frame_batch_run_sub1, command = isChecked_PAT_b, text='PAT batch run', variable=batch_run_gui, onvalue= True, offvalue= False)
check_batch_run.grid(column = 0, row = 0, stick = "NSEW", padx = 5, pady = 5)

def openfile_batch():
    batch_file_gui = tk.filedialog.askopenfilename(title="Open file")
    entry_batch_file.delete(0, tk.END)
    entry_batch_file.insert(tk.END, batch_file_gui) # add this

### Button
button_batch_file = tk.Button(master = frame_batch_run_sub2, text='Browse for PAT batch file', width=30, command=openfile_batch, state = "disable").grid(column = 0, row = 1, stick = "NSEW", padx = 5, pady = 5)

### Entry
entry_batch_file = tk.Entry(master = frame_batch_run_sub2, width = 50, state = "disable")
entry_batch_file.grid(column = 1, row = 1, stick = "NSEW", padx = 5, pady = 5)

frame_batch_run_sub1.grid(column = 0, row = 0, stick = "NSEW")
frame_batch_run_sub2.grid(column = 0, row = 1, stick = "NSEW")

# In[]:
# PAT Single Run
frame_single_run = tk.LabelFrame(master = window, text = "PAT Single Run")
frame_single_run_sub1 = tk.Frame(master = frame_single_run)
frame_single_run_sub2 = tk.Frame(master = frame_single_run)

### Check button
def isChecked_PAT_s():
    if single_run.get() == True:
        for child in frame_single_run_sub2.winfo_children():
            child.configure(state='normal')
    elif single_run.get() == False:
        for child in frame_single_run_sub2.winfo_children():
            child.configure(state='disable')


single_run = tk.BooleanVar()

check_single_run = tk.Checkbutton(master = frame_single_run_sub1, command = isChecked_PAT_s, text='PAT single run', variable=single_run, onvalue= True, offvalue= False)
check_single_run.grid(stick = "NSEW", column = 0, row = 0, pady = 5, padx = 5)

## Run name
label_runname = tk.Label(master = frame_single_run_sub2, text = 'Run name')
label_runname.configure(state="disable")
label_runname.grid(column = 0, row = 0, stick = "NSEW", pady = 5, padx = 5)
entry_runname = tk.Entry(master = frame_single_run_sub2, width = 40, state = "disable")
entry_runname.grid(column = 1, row = 0, stick = "NSEW", pady = 5, padx = 5)

## zts file
def openfile_zts():
    zts_file = tk.filedialog.askopenfilename(title="Open file")
    entry_zts_file.delete(0, tk.END)
    entry_zts_file.insert(tk.END, zts_file) # add this

### Button
button_zts_file = tk.Button(master = frame_single_run_sub2, text='Browse for .zts file', width=30, command=openfile_zts, state = "disable").grid(column = 0, row = 1, stick = "NSEW", padx = 5, pady = 5)

### Entry
entry_zts_file = tk.Entry(master = frame_single_run_sub2, width = 50, state = "disable")
entry_zts_file.grid(column = 1, row = 1, stick = "NSEW", padx = 5, pady = 5)

## swi file
def openfile_swi():
    pwc_input = tk.filedialog.askopenfilename(title="Open file")
    entry_swi_file.delete(0, tk.END)
    entry_swi_file.insert(tk.END, pwc_input) # add this

### Button
button_swi_file = tk.Button(master = frame_single_run_sub2, text='Browse for .PWC file', width=30, command=openfile_swi, state = "disable").grid(column = 0, row = 2, stick = "NSEW", padx = 5, pady = 5)

### Entry
entry_swi_file = tk.Entry(master = frame_single_run_sub2, width = 50, state = "disable")
entry_swi_file.grid(column = 1, row = 2, stick = "NSEW", padx = 5, pady = 5)

## wetland (custom) .csv file
def openfile_wetland():
    semi_csv = tk.filedialog.askopenfilename(title="Open file")
    entry_wetland_file.delete(0, tk.END)
    entry_wetland_file.insert(tk.END, semi_csv) # add this

### Button
button_wetland_file = tk.Button(master = frame_single_run_sub2, text='Browse for wetland (custom) .csv file', width=30, command=openfile_wetland, state = "disable").grid(column = 0, row = 3, stick = "NSEW", padx = 5, pady = 5)

### Entry
entry_wetland_file = tk.Entry(master = frame_single_run_sub2, width = 50, state = "disable")
entry_wetland_file.grid(column = 1, row = 3, stick = "NSEW", padx = 5, pady = 5)

## pond .csv file
def openfile_pond():
    aq_csv = tk.filedialog.askopenfilename(title="Open file")
    entry_pond_file.delete(0, tk.END)
    entry_pond_file.insert(tk.END, aq_csv) # add this

### Button
button_pond_file = tk.Button(master = frame_single_run_sub2, text='Browse for pond .csv file', command=openfile_pond, state = "disable", width = 30).grid(column = 0, row = 4, stick = "NSEW", padx = 5, pady = 5)

### Entry
entry_pond_file = tk.Entry(master = frame_single_run_sub2, width = 50, state = "disable")
entry_pond_file.grid(column = 1, row = 4, stick = "NSEW", padx = 5, pady = 5)

## application method

### Label
label_app_method = tk.Label(master = frame_single_run_sub2, text = 'Application method')
label_app_method.configure(state="disable")
label_app_method.grid(column = 0, row = 5, stick = "NSEW", padx = 5, pady = 5)

### OptionMenu
app_method_options = ["None",
                      "Aerial_Very_Fine_to_Fine",
                      "Aerial_Fine_to_Medium",
                      "Aerial_Medium_to_Coarse",
                      "Aerial_Coarse_to_Very_Coarse",
                      "Ground_Very_Fine_to_Fine_High Boom_90th",
                      "Ground_Very_Fine_to_Fine_Low_Boom_90th",
                      "Ground_Very_Fine_to_Fine_High_Boom_50th",
                      "Ground_Very_Fine_to_Fine_Low_Boom_50th",
                      "Ground_Fine_to_Medium/Coarse_High_Boom_90th",
                      "Ground_Fine_to_Medium/Coarse_Low_Boom_90th",
                      "Ground_Fine_to_Medium/Coarse_High_Boom_50th",
                      "Ground_Fine_to_Medium/Coarse_Low_Boom_50th",
                      "Airblast_Normal",
                      "Airblast_Dense",
                      "Airblast_Sparse",
                      "Airblast_Vineyard",
                      "Airblast_Orchard",
                      "User_Defined"]

app_method_gui = tk.StringVar()

optionm_application_method = tk.OptionMenu(frame_single_run_sub2, app_method_gui, *app_method_options)
optionm_application_method.config(width=40, state='disable')
optionm_application_method.grid(column=1, row=5, stick="NSEW", padx=5, pady=5)

# In[]:
# Buffer distance
### Label
label_buffer = tk.Label(master = frame_single_run_sub2, text = 'Buffer distance (m)')
label_buffer.configure(state="disable")
label_buffer.grid(column = 0, row = 6, padx=5, pady=5, stick = "NSEW")
### Entry
entry_buffer = tk.Entry(master = frame_single_run_sub2, width = 50)
entry_buffer.insert(0,0)
entry_buffer.configure(state="disable")
entry_buffer.grid(column = 1, row = 6, padx=5, pady=5, stick = "NSEW")

# Formation decline

def open_popup():
   fd_warning= tk.Toplevel(window)
   fd_warning.geometry("250x150")
   fd_warning.title("Formation decline WARNING")
   tk.Label(fd_warning, text= "WARNING: PAT does not calculate formation decline, meaning there will be no additional degredation of parent after PWC. This could result in underestimation of metabolites. For more information please contact Jerrett Fowler (fowler.jerrett@epa.gov)", wraplength=225, justify = "left").grid(column = 1, row = 6, padx=5, pady=5, stick = "NSEW")

### Label
label_fd = tk.Label(master = frame_single_run_sub2, text = '***Formation decline***')
label_fd.configure(state="disable")
label_fd.grid(column = 0, row = 7, padx=5, pady=5, stick = "NSEW")

### OptionMenu
fd_options = ["Parent",
              "Daughter",
              "Granddaughter"]

fd_gui = tk.StringVar()

optionm_fd = tk.OptionMenu(frame_single_run_sub2, fd_gui, *fd_options)
optionm_fd.config(width=40, state='disable')
optionm_fd.grid(column=1, row=7, stick="NSEW", padx=5, pady=5)

frame_single_run_sub1.grid(stick = "NSEW")
frame_single_run_sub2.grid(stick = "NSEW")

# In[]:
# PWC batch run

frame_pwc_batch_run = tk.LabelFrame(master = window, text = "PWC Run Info")

frame_pwc_batch_run_sub1 = tk.Frame(master = frame_pwc_batch_run)
frame_pwc_batch_run_sub2 = tk.Frame(master = frame_pwc_batch_run)

def isChecked_PWC_b():
    if use_pwc_ex_batch_gui.get() == True or use_pwc_int_batch_gui.get() == True:
        for child in frame_pwc_batch_run_sub1.winfo_children():
            child.configure(state='normal')
    else:
        for child in frame_pwc_batch_run_sub1.winfo_children():
            child.configure(state='disable')

use_pwc_ex_batch_gui = tk.BooleanVar()
use_pwc_int_batch_gui = tk.BooleanVar()
use_pwc_single_gui = tk.BooleanVar()

check_pwc_ex_batch_run = tk.Checkbutton(master = frame_pwc_batch_run_sub2, command = isChecked_PWC_b, text='PWC external batch run', variable=use_pwc_ex_batch_gui, onvalue= True, offvalue= False)
check_pwc_ex_batch_run.grid(column=0, row=0, pady=5, padx=5, stick="NSEW")

check_pwc_in_batch_run = tk.Checkbutton(master = frame_pwc_batch_run_sub2, command = isChecked_PWC_b, text='PWC internal batch run', variable=use_pwc_int_batch_gui, onvalue= True, offvalue= False)
check_pwc_in_batch_run.grid(column=1, row=0, pady=5, padx=5, stick="NSEW")

check_pwc_single_run = tk.Checkbutton(master = frame_pwc_batch_run_sub2, command = isChecked_PWC_b, text='PWC single run', variable=use_pwc_single_gui, onvalue= True, offvalue= False)
check_pwc_single_run.grid(column=2, row=0, pady=5, padx=5, stick="NSEW")

# rbutton_pwc_ex_batch_run = tk.Radiobutton(master = frame_pwc_batch_run_sub2, text='PWC external batch run', command = isChecked_PWC_b, variable=pwc_run_info_gui, value = "external")
# rbutton_pwc_in_batch_run = tk.Radiobutton(master = frame_pwc_batch_run_sub2, text='PWC internal batch run', command = isChecked_PWC_b, variable=pwc_run_info_gui, value = "internal")
# rbutton_pwc_single_run = tk.Radiobutton(master = frame_pwc_batch_run_sub2, text='PWC single run', variable=pwc_run_info_gui, value = "single")

# rbutton_pwc_ex_batch_run.grid(column=0, row=0, pady=5, padx=5, stick="NSEW")
# rbutton_pwc_in_batch_run.grid(column=1, row=0, pady=5, padx=5, stick="NSEW")
# rbutton_pwc_single_run.grid(column=2, row=0, pady=5, padx=5, stick="NSEW")

# PWC scenario
frame_pwc_scenarios = tk.Frame(master = frame_pwc_batch_run)

def openfile_pwc_scenarios():
    scen_dir = tk.filedialog.askdirectory(title="Open file")
    entry_pwc_batch_scenarios.delete(0, tk.END)
    entry_pwc_batch_scenarios.insert(tk.END, scen_dir) # add this

### Button
button_pwc_batch_scenarios = tk.Button(master = frame_pwc_batch_run_sub1, width = 30, text='Browse for PWC scenarios', command=openfile_pwc_scenarios, state="disable").grid(column=0, row=2, stick="NSEW",pady=5, padx=5)

### Entry
entry_pwc_batch_scenarios = tk.Entry(master = frame_pwc_batch_run_sub1, width = 50, state="disable")
entry_pwc_batch_scenarios.grid(column=1, row=2, sticky="NSEW", padx=5, pady=5)
frame_pwc_batch_run_sub2.grid(column=0, row=0, stick = "NSEW")
frame_pwc_batch_run_sub1.grid(column=0, row=1, stick = "NSEW")

# In[]:
### Endpoints
frame_endpoints = tk.LabelFrame(master = window, text = "Endpoints")

def openfile_endpoints():
    endpoints = tk.filedialog.askopenfilename(title="Open file")
    entry_endpoints.delete(0, tk.END)
    entry_endpoints.insert(tk.END, endpoints) # add this

### Button
button_endpoints = tk.Button(master = frame_endpoints, width = 30, text='Browse for endpoints file', command=openfile_endpoints).grid(column=0, row=0, stick="NSEW", pady=5, padx=5)

### Entry
entry_endpoints = tk.Entry(master = frame_endpoints, width = 50)
entry_endpoints.grid(column=1, row=0, stick="NSEW", padx=5, pady=5)

# In[]:
### Run button
frame_run = tk.Frame(master = window)

def run_PAT():
    label_runPAT.config(text="Running...")
    if fd_gui.get() == "Parent":
        fd = 0
    elif fd_gui.get() == "Daughter":
        fd = 1
        open_popup()
    elif fd_gui.get() == "Granddaughter":
        fd = 2
        open_popup()
    else:
        fd = -1
    input_dir = entry_input_dir.get()
    output_dir = os.path.join(input_dir, entry_output_dir.get())
    make_figs = make_figs_gui.get()
    batch_run = batch_run_gui.get()
    batch_file = entry_batch_file.get()
    use_pwc_batch = use_pwc_ex_batch_gui.get()
    use_pwc_internal_batch = use_pwc_int_batch_gui.get()
    scen_dir = entry_pwc_batch_scenarios.get()
    sdf_file = os.path.join(input_dir, "sdf.csv")
    endpoints = entry_endpoints.get()
    run_name = os.path.join(output_dir, entry_runname.get())
    zts_file = entry_zts_file.get()
    pwc_input = entry_swi_file.get()
    semi_csv = entry_wetland_file.get()
    aq_csv = entry_pond_file.get()
    app_method = app_method_gui.get()
    options = ""
    if terrestrial.get() == True:
        options = "t"
    if aquatic.get() == True:
        options = options + "a"
    if semiaquatic.get() == True:
        options = options + "s"
    esa = esa_gui.get()
    buffer = float(entry_buffer.get())
    save_terr = save_terr_gui.get()
    save_saq = save_saq_gui.get()
    patv2_7_1.main(input_dir, output_dir, batch_run, make_figs, batch_file, use_pwc_batch, scen_dir, sdf_file, endpoints, run_name, zts_file, pwc_input, semi_csv, aq_csv, app_method, options, esa, buffer, save_terr, save_saq, fd, use_pwc_internal_batch)
    label_runPAT.config(text = "Finished!")

### button
button_runPAT = tk.Button(master = frame_run, width = 30,  height = 5, text='Run PAT', command=run_PAT).grid(column=0, row=0, stick="NSEW", padx=5, pady=5)

### label
label_runPAT = tk.Label(master = frame_run, text='')
label_runPAT.grid(column = 1, row=0, stick="NSEW", pady=5, padx=5)

# In[]:
## Column 0
frame_run_info.grid(column = 0, row = 0, columnspan = 1, rowspan = 3, padx = 5, pady=5, stick = "NSEW")
frame_input_dir.grid(column = 0, row = 3, columnspan = 1, rowspan = 2, padx=5, pady=5, stick = "NSEW")
frame_endpoints.grid(column = 0, row=5, columnspan = 1, rowspan = 1, padx=5, pady=5, stick="NSEW")
frame_pwc_batch_run.grid(column = 0, row = 8, columnspan = 1, rowspan = 2, padx=5, pady=5, stick = "NSEW")
frame_batch_run.grid(column = 0, row = 6, columnspan = 1, rowspan = 2, padx=5, pady=5, stick = "NSEW")
#frame_buffer.grid(column =2, row = 0, rowspan=1, columnspan=1, pady=5, padx=5, stick="NSEW")
frame_single_run.grid(column = 2, row = 0, columnspan =1, rowspan = 7, padx = 5, pady = 5, stick = "NSEW")
## Column 2
frame_run.grid(column=2, row=8, columnspan=1, rowspan=3, padx=5, pady=5, stick="NSEW")

window.mainloop()
