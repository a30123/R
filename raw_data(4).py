# -*- coding: utf-8 -*-
"""
Created on Sun Jan 17 15:14:20 2016

@author: Mary
"""
#########################################################################################################
###      #####  #####        #####       ###############    #   ###  ###       ###       ################
###  #########  ########  ########  ####################  #  #  ###  ###  ###  ###  ###  ################
###  #########  ########  ########  ####################  ####  ###  ###  ###  ###  ###  ################
###      #####  ########  ########       ###############  ####  ###  ###       ###       ################
#########################################################################################################
import os

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt 

class RunData:
    def __init__(self, run_no, sensor_name, physmax, step_values, current_values, deviation_values,  setpoint_values = None):
        self.sensor_name = sensor_name        
        self.current_values = current_values
        self.deviation_values = deviation_values
        self.setpoint_values = setpoint_values
        self.step_values = step_values
        self.physmax = physmax
        self.segment_category = None
        
    def reconstruct_setpoint(self):
        self.setpoint_values = self.current_values- self.deviation_values * self.physmax/100
#        return self.setpoint_values
        
    def adjusted_length(self):
        min_step = min(self.step_values)
        if min_step == 0:
            for i in range(len(self.step_values)):
                if i == 0 and int(self.step_values[i]) == min_step:
                    start_index = np.count_nonzero(self.step_values == min_step)
                    break
                else:
                    if int(self.step_values[i]) == min_step:
                        start_index = i + np.count_nonzero(self.step_values == min_step)
                        break
        else:
            for i in range(len(self.step_values)):
                if int(self.step_values[i]) == min_step:
                    start_index = i
                    break
    
        max_step = max(self.step_values[start_index:])
        for i in reversed(range(int(start_index), len(self.step_values))):
            if int(self.step_values[i]) == int(max_step):
                break  
        end_index = i + 1
                 
        return start_index, end_index
    
    def segment_type(self):
        start_idx, end_idx = self.adjusted_length()
        adjusted_step_values = self.step_values[start_idx: end_idx]
        adjusted_setpoint_values = self.setpoint_values[start_idx: end_idx]
        setpoint_values_difference = np.round(adjusted_setpoint_values[1:] - adjusted_setpoint_values[: -1], 1)
        
        flat_data_collection_start = []
        flat_data_collection_end = []
        segment_type = []
        segment_start = []
        segment_end = []
        
        flat_idx = np.where(setpoint_values_difference == 0)[0]
        flat_data_collection_start.append(flat_idx[0])
        for i in range(len(flat_idx) -1 ):
            if flat_idx[i + 1] - flat_idx[i] > 1:
                flat_data_collection_end.append(flat_idx[i] + 1)
                flat_data_collection_start.append(flat_idx[i + 1])
        flat_data_collection_end.append(flat_idx[-1] + 1)
        
        real_flat_idx = np.where(np.asarray(flat_data_collection_end) - np.asarray(flat_data_collection_start) >= 5)[0]
        for j in range(len(real_flat_idx)):
            segment_type.append(4)
            segment_start.append(flat_data_collection_start[real_flat_idx[j]])
            segment_end.append(flat_data_collection_end[real_flat_idx[j]])
        
        try:
            nonflat_data_collection_start = []
            nonflat_data_collection_end = []
            
            for k in range(len(real_flat_idx)):
                if k == 0:
                    if segment_start[k] == 0:
                        nonflat_data_collection_start.append(segment_end[k] + 1)
                        nonflat_data_collection_end.append(segment_start[k + 1] - 1)
                    else:
                        nonflat_data_collection_start.append(0)
                        nonflat_data_collection_end.append(segment_start[k] - 1)
                elif k == len(real_flat_idx) - 1:
                    if segment_end[k] != end_idx - start_idx:
                        nonflat_data_collection_start.append(segment_end[k] + 1)
                        nonflat_data_collection_end.append(end_idx - start_idx)
                else:
                    nonflat_data_collection_start.append(segment_end[k] + 1)
                    nonflat_data_collection_end.append(segment_start[k + 1] - 1)
            
            for j in range(len(nonflat_data_collection_start)):
                nonflat_step, nonflat_step_count = np.unique(adjusted_step_values[nonflat_data_collection_start[j]: nonflat_data_collection_end[j] + 1], return_counts=True)
                for k in range(len(nonflat_step)):
                    if k == 0:
                        segment_start.append(nonflat_data_collection_start[j])
                        segment_end.append(nonflat_data_collection_start[j] + nonflat_step_count[k] - 1)
                    else:
                        segment_start.append(segment_end[-1] + 1)
                        segment_end.append(segment_end[-1] + nonflat_step_count[k])
                    
                    slope = (adjusted_setpoint_values[segment_end[-1]] - adjusted_setpoint_values[segment_start[-1]]) / (segment_end[-1] - segment_start[-1])
                    if slope < -0.1:
                        segment_type.append(2)
                    elif slope >= -0.1 and slope < -0.01:
                        segment_type.append(6)
                    elif slope > 0.1:
                        segment_type.append(3)
                    elif slope <= 0.1 and slope > 0.01:
                        segment_type.append(5)
                    else:
                        segment_type.append(7)
            
            segment = np.column_stack((segment_type, np.asarray(segment_start) + np.asarray(start_idx)))
            segment = np.column_stack((segment, np.asarray(segment_end) + np.asarray(start_idx)))
            segment = np.asarray(sorted(segment, key = lambda x : x[2]))
            
#            modified_segment =  self.segment_simplify()           
            
            return segment
        except:
            segment = np.column_stack((segment_type, np.asarray(segment_start) + np.asarray(start_idx)))
            segment = np.column_stack((segment, np.asarray(segment_end) + np.asarray(start_idx)))
            segment = np.asarray(sorted(segment, key = lambda x : x[2]))
            
            return segment
    
    def segment_simplify(self):
        segment = self.segment_type() 
        same_type_idx = []
        region_type = [2, 3, 5, 6]
        for number in region_type:
            simplified_idx = np.where(segment[:, 0] == number)[0]
            i = 0
            j = i
            while j < len(simplified_idx)-1:
                if segment[simplified_idx[j + 1], 1] - segment[simplified_idx[i], 2] == 1:
                    segment[simplified_idx[i], 2] = segment[simplified_idx[j + 1], 2]
                    same_type_idx.append(simplified_idx[j + 1])
                    j=j + 1
                else:
                    i = j + 1
                    j = i
            
        modified_segment = []
        for j in range(len(segment)):
            if j not in same_type_idx:
                modified_segment.append(segment[j,:])
        modified_segment = np.array(modified_segment)
        
        return modified_segment
    
    def segment_plot(self):
        segment = self.segment_simplify()
        start_idx, end_idx = self.adjusted_length()
        
        fig = plt.figure(figsize=(14,6))
        ax1 = fig.add_subplot(111)
        line1 = ax1.plot(self.current_values[start_idx: end_idx], color = 'g', linewidth = 2, label = 'current')
        line2 = ax1.plot(self.setpoint_values[start_idx: end_idx], color = 'r', linestyle = '--', linewidth = 2, label = 'setpoint')
        
        for i in range(len(segment)):
            if segment[i, 0] == 2:
                plt.axvspan(segment[i, 1], segment[i, 2], facecolor = 'r', alpha = 1)
            elif segment[i, 0] == 3:
                plt.axvspan(segment[i, 1], segment[i, 2], facecolor = 'y', alpha = 1)
            elif segment[i, 0] == 4:
                plt.axvspan(segment[i, 1], segment[i, 2], facecolor = 'c', alpha = 0.5)
            elif segment[i, 0] == 5:
                plt.axvspan(segment[i, 1], segment[i, 2], facecolor = 'm', alpha = 0.5)
            elif segment[i, 0] == 6:
                plt.axvspan(segment[i, 1], segment[i, 2], facecolor = 'k', alpha = 0.2)
        
        ax1.set_xlim(0, len(self.setpoint_values) - 1)
        ax1.set_ylim(-0.1 * self.physmax, 1.3 * self.physmax)
        if self.sensor_name == 'Heater.temp':
            ax1.set_ylabel('Temperature (\u00b0' + 'C)', fontsize = 16)
        else:
            ax1.set_ylabel('Flow (sccm)', fontsize = 16)
        ax1.set_xlabel('Time(second)', fontsize = 16)
#        ax1.legend(loc = 2, fontsize = 14)
        
        ax2 = ax1.twinx()
        line3 = ax2.plot(self.deviation_values[start_idx: end_idx], color = 'b', linewidth = 2, label = 'deviation')
        ax2.set_ylabel('deviation', fontsize = 16)
#        ax2.legend(loc = 0, fontsize = 14)

        lines = line1 + line2 + line3
        labs = [l.get_label() for l in lines]
        ax1.legend(lines, labs, bbox_to_anchor=(0., 1.02, 1., .102), loc = 3, ncol = 3, mode = "expand", borderaxespad = 0., fontsize = 14)
#        ax1.legend(lines, labs, bbox_to_anchor=(1.05, 1), loc=2, borderaxespad=0., fontsize = 14)

        ax1.grid()
        plt.show()
        
#    def __str__(self):
#        return 'RunData({0}, {1}, {2})'.format(
#            self.name, self.number, self.balance)
            
if __name__ == "__main__":
    files_folder = "D://Heater//cycle_data//current"
    files_folder2 = "D://Heater//cycle_data//deviation"
    config_file_path_name = os.path.join(os.getcwd(),"parameter.csv")
    
    sensor= 'Heater.temp'
   
    files_in_folder = os.listdir(files_folder)
    temp_filename = files_in_folder[4]
    
    A = pd.read_csv(os.path.join(files_folder,temp_filename))
    B = pd.read_csv(os.path.join(files_folder2,temp_filename.replace('-current.csv','-deviation.csv')))
    C = np.genfromtxt(config_file_path_name,dtype = [('Name','S20'),('Physmax','i16')], deletechars = """~!@#$%^&*()-=+~\|]}[{';: /?>,<""",delimiter = ",",skip_header = 1)    
    
    cc=dict(C)
    PhysMax = cc[sensor.encode('UTF-8')]    
    
    current_values = np.array(A[sensor])
    deviation_values = np.array(B[sensor]) # matrix where each row is a vector that represent a digit.
    step_values = np.array(A['Step'])
    
    
    new_data = RunData(1, sensor, PhysMax, step_values, current_values, deviation_values,  setpoint_values = None)
    new_data.reconstruct_setpoint()
    new_data.segment_plot()

    

    
    