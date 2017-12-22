####################################################
#############  daVinci		    #####################
####################################################

# S. Jahfari 04-2011

####################################################
########## Basic preferences  ######################
####################################################

# knoppen + responsies
scenario = "face_stop";
response_matching = simple_matching;
active_buttons = 2;
button_codes=1,2;
target_button_codes = 1,2;
default_stimulus_time_in = 0;
default_stimulus_time_out = never;  
default_background_color = 0,0,0;
default_all_responses = false;

# log waarden
no_logfile = false;   
#event_code_delimiter = "\t";
response_logging = log_active;	

# imaging variables
scenario_type = fMRI;  
#scenario_type = fMRI_emulation;
scan_period = 2000;   
pulses_per_scan = 1; 
pulse_code = 255;	


####################################################
##################### Begin SDL ####################
####################################################

# house = left, face = right # practice block

begin;

#read instruction picture

	#read instruction picture


		
	
# set up sound variable
	sound {wavefile{ filename = "bleep2.wav"; };} beep;

# Read in Fixation 
	picture {bitmap{filename = "fix.jpg";scale_factor = 0.3;
				}; x=0; y=0;} default;
				

################################################################################
######################### Read in Picture stimuli ##############################
################################################################################

	# stimuli names should be changed!
	# ongefilterd
	array {

		bitmap{filename = "V2.jpg";scale_factor = 0.5; }"v_2";
		bitmap{filename = "V3.jpg";scale_factor = 0.5; }"v_3";
		bitmap{filename = "V5.jpg";scale_factor = 0.5; }"v_5";
		bitmap{filename = "V6.jpg";scale_factor = 0.5; }"v_6";
		bitmap{filename = "V8.jpg";scale_factor = 0.5; }"v_8";
		bitmap{filename = "V10.jpg";scale_factor = 0.5; }"v_10";
		bitmap{filename = "V13.jpg";scale_factor = 0.5; }"v_13";
		bitmap{filename = "V14.jpg";scale_factor = 0.5; }"v_14";
		bitmap{filename = "V15.jpg";scale_factor = 0.5; }"v_15";

		
		}vrouw_array;


	array {

		bitmap{filename = "M2.jpg";scale_factor = 0.5; }"m_2";
		bitmap{filename = "M5.jpg";scale_factor = 0.5; }"m_5";
		bitmap{filename = "M6.jpg";scale_factor = 0.5; }"m_6";
		bitmap{filename = "M8.jpg";scale_factor = 0.5; }"m_8";
		bitmap{filename = "M9.jpg";scale_factor = 0.5; }"m_9";
		bitmap{filename = "M11.jpg";scale_factor = 0.5; }"m_11";
		bitmap{filename = "M12.jpg";scale_factor = 0.5; }"m_12";
		bitmap{filename = "M13.jpg";scale_factor = 0.5; }"m_13";
		bitmap{filename = "M14.jpg";scale_factor = 0.5; }"m_14";
		
		}man_array;
	
			
######################################################################################
############## feedback pictures #####################################################
######################################################################################

# feedback for practice trials ppn
		text {caption = "te langzaam!";font_size = 30;font_color=255,255,255;}telaattext;
			picture {text telaattext; x=0;y=0;}telaatpic;
			
		text {caption = "miss!";font_size = 30;font_color=255,255,255;}misstext;
			picture {text misstext; x=0;y=0;}misspic;
			
		
			
		picture{	bitmap{filename = "mlvr.bmp"; }; x=0; y=0;} mlvr;
		
		picture{	bitmap{filename = "vlmr.bmp"; }; x=0; y=0;} vlmr;
		
######################################################################################
################### instruction pictures #############################################
######################################################################################

text {caption = "einde taak druk op een knop om verder te gaan.";font_size = 20;font_color=255,255,255;}e1text;
			picture {text e1text; x=0;y=0;}e1pic;

#######################################################################################		
##################### Define Trial outline ############################################
#######################################################################################


# Go Trial

	trial {
		trial_duration = 4000;
		trial_type = fixed;
		
		stimulus_event{
				picture default;
				time = 0;
				duration = next_picture;
				response_active = false;
			}jitter;
	
		stimulus_event{
			picture 
				{  
           			 box { height = 1; width = 1; color = 0,0,0; };
				x = 0; y = 0;
			}pic1;
			time = 500;
			duration = 500;
			target_button = 1;
			code = 0;
		}stim1;

		
	}Go_trial; 


	trial {
		trial_duration = 4000;
		trial_type = fixed;
		
		stimulus_event{
				picture default;
				time = 0;
				duration = next_picture;
				response_active = false;
			}jitter2;
		
		stimulus_event
				{
				picture 
					{  
					box { height = 1; width = 1; color = 0,0,0; };
					x =0; y = 0;
				}pic2;
				time = 500;
				duration = 500;
				response_active = true;
				target_button = 1;
				code = 1;
			}stim2;
		
		stimulus_event
				{
				sound beep;
				deltat = 250;
				duration = 1000;
				response_active = false;
			}stp;
			
	
	}Stop_trial; 
	
################## define feedback trials ##########

############# make feedback, intro and side slides ########################


# instruciotn trials for side (m = left or rigth)

# instruciotn trials for side (m = left or rigth)

# instructie trial
	trial {   
   picture mlvr;
   time = 0;       
   duration = next_picture;
   picture default;
   mri_pulse = 1;   
   duration = next_picture; 
   code="mlvr";
      } mlvrslide;
      
   trial {   
   picture vlmr;
   time = 0;       
   duration = next_picture;
   picture default;
   mri_pulse = 1;   
   duration = next_picture; 
   code="vlmr";
      } vlmrslide;
      
      
# feedback trials

	trial {
		trial_duration = 2000;
		trial_type = fixed;
		picture telaatpic;
		time = 0;       
		duration = next_picture;   
		code="te laat";
			} TelaatSlide;

# miss slide

		trial {
		trial_duration = 2000;
		trial_type = fixed;
		picture misspic;
		time = 0;       
		duration = next_picture;   
		code="miss";
			} MissSlide;	
			

      
##### null trial      
   trial {  
	trial_duration = 4000;
	trial_type = fixed;
	stimulus_event
			{ 
				picture default;
				time = 0;       
				duration = 2000;
				code="null";
			} blank_event;
      } Null1;

# instruction trial
     
# einde trial
	 trial { 
   
			picture e1pic;
		time = 0;       
		duration = response;
		target_button=1;
		code="einde1";
			
   
      } einde1Slide;	
      
####################################################
##################### Begin PCL ####################
####################################################


begin_pcl;

##########################################################################################
##########################################################################################
####### PCL practice block  #########################################################
##########################################################################################
##########################################################################################


##########################################################################################
##########################################################################################
####### PCL Block 1 Stop face 150  #########################################################
##########################################################################################
##########################################################################################


##### define logfile variables #####################

output_file out1 = new output_file;
out1.open_append(logfile.subject() + "_face1scanner_150stop.txt"); 
out1.print( "\nTrial\tGoStop\tRes\tResT\tSSD1\tS1\tS_R1\tP1\tmiss1\tRT\tTime\n");  

##### present instruction trial #####################
##### present instruction trial #####################

# Man = links, Vrouw= rechts
vlmrslide.present();

####################################################
## read in variables of interest for bloc1_session##
####################################################


array <int> iTr1[150];  # 0=Go, 1=Stop
array <int> iSide1[150]; # response Side (left, right)
array <int> iJit1[150]; # Jitter
array <int> iNull1[150]; # Trial type


input_file trial2 = new input_file;				
trial2.open("\\scripts\\stop150_" + logfile.subject() +".txt");			
loop int c = 1 until c> 150

begin;
iTr1[c] = trial2.get_int();				
iSide1[c] = trial2.get_int();			
iJit1[c] = trial2.get_int();  
iNull1[c] = trial2.get_int();			
  		
c=c+1;
end;
	
trial2.close();	
		


####################################################
## define trial seq for trial session and start   ##
####################################################

	 int S1_1 = 0;
	 double P1_1 = 0.0 ;
	 int SSD1_1 = 250 ;
	 int S_R1_1 = 1 ;
	 int miss1_1 = 0; 
	 
		 
	stimulus_data last2;	 
	 int RT2;
	 int Res2;
	 int ResT2;
	 int Time2;
	 
	loop 
	 int r = 1; 
	until r>150
	
	begin;
	
		begin;
		
			# shuffle pictures
			vrouw_array.shuffle(1,vrouw_array.count());
			man_array.shuffle(1,man_array.count());
		
		
		# set response buttons (1=links, 2=rechts, man=1, vrouw=2)
			if iSide1[r]==1 then
				stim1.set_target_button(1); 
				stim2.set_target_button(1); 
				pic1.set_part(1, vrouw_array[1] ); 
				pic2.set_part(1, vrouw_array[1] ); 
				string filename = vrouw_array[1].filename();
				stim1.set_event_code(filename);
				stim2.set_event_code(filename);
			elseif iSide1[r]==2 then
				stim1.set_target_button(2);
				stim2.set_target_button(2); 
				pic1.set_part(1, man_array[1] ); 
				pic2.set_part(1, man_array[1] ); 
				string filename = man_array[1].filename();
				stim1.set_event_code(filename);
				stim2.set_event_code(filename);
			end;
			
	
			
		######## begin trial definition #########################
		
	
		
		# set jitter time
		stim1.set_time(iJit1[r]);
		stim2.set_time(iJit1[r]);
		
		
		if iTr1[r]== 1 then
			S1_1 = S1_1 + 1;
			P1_1 = double(S_R1_1) /double(S1_1);
		end;
		
		
		# set stop signal delay sep for each stimuli filter
		if iTr1[r]==1 && P1_1< 0.5 then
			SSD1_1 = SSD1_1 + 50;
		elseif iTr1[r]==1 && P1_1 > 0.5 && SSD1_1 > 50 then
			SSD1_1 = SSD1_1 - 50;
		elseif iTr1[r]==1 && P1_1 > 0.5 && SSD1_1 <= 50 then
			SSD1_1 = SSD1_1;
		end;
		
		
		if iTr1[r]==0 then
			Go_trial.present();
		elseif iTr1[r]==1 then
			stp.set_deltat(SSD1_1);
			Stop_trial.present();
			miss1_1 = miss1_1 + response_manager.misses ();
			S_R1_1 = S1_1 - miss1_1;
		end;
		
		
		last2 = stimulus_manager.last_stimulus_data();
		RT2 = last2.reaction_time();
		Res2 =last2.button();
		ResT2 = last2.type();
		
		
		if iTr1[r]  == 0 && RT2 == 0 then
			MissSlide.present();	
		end;
		
		
		
			out1.print(r);					out1.print("\t");
			out1.print(iTr1[r]);			out1.print("\t");
			out1.print(Res2);	   			out1.print("\t");
			out1.print(ResT2);	   			out1.print("\t");
			
		if iTr1[r] == 1 then
			out1.print(SSD1_1);			out1.print("\t");
			out1.print(S1_1);			out1.print("\t");
			out1.print(S_R1_1);			out1.print("\t");
			out1.print(P1_1);			out1.print("\t");
			out1.print(miss1_1);		out1.print("\t");
					
		else
			out1.print("99");		out1.print("\t");
			out1.print("99");		out1.print("\t");
			out1.print("99");		out1.print("\t");
			out1.print("99");		out1.print("\t");
			out1.print("99");		out1.print("\t");
			
		end;
		
		out1.print(RT2);		out1.print("\t");
		out1.print(Time2);   out1.print("\n");
		
		if iNull1[r]==5 then
		Null1.present();
		end;
		
		r=r+1
		end;
end;

out1.close();

einde1Slide.present();