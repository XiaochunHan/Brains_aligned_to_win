clear;clc;
basedir = '/Users/xiaochunhan/Documents/Research/Projects/Brains_align_to_win/Data/Develop';
metafile = 'Real_WINS_BINS_FC_BOLD_oneF.mat';
reffile = 'BINS_ind_data.mat';
%%
load(fullfile(basedir, reffile));
title = BINS_data.title([1:3,5:31]);
clear BINS_data
%%
load(fullfile(basedir, metafile));
bins = array2table(BINS_data, 'VariableNames', title);
ins = array2table(INS_data, 'VariableNames', title);
bold = array2table(BOLD_data, 'VariableNames', title);
fc = array2table(FC_data, 'VariableNames', title);
writetable(bins, fullfile(basedir,'BINS_single_trial_invest_oneF.csv'),"Delimiter",",");
writetable(ins, fullfile(basedir,'INS_single_trial_invest_oneF.csv'),"Delimiter",",");
writetable(bold, fullfile(basedir,'BOLD_single_trial_invest_oneF.csv'),"Delimiter",",");
writetable(fc, fullfile(basedir,'FC_single_trial_invest_oneF.csv'),"Delimiter",",");

fprintf('Done!\n');