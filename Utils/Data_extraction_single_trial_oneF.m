clear;clc;
basedir = '/Users/xiaochunhan/Documents/Research/Projects/Brains_align_to_win/Data/Develop';
metafile = 'Real_WINS_BINS_FC_BOLD_oneF.mat';
reffile = 'BINS_ind_data.mat';
behfile = 'Real_investment_oneF.mat';
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
%%
load(fullfile(basedir,behfile));
for i = 1:24
    round{i} = ['R',num2str(i),'_inv'];
end
title_beh = ['n','Type(Bonding=2)','num','Gender(M=1)','role(A=1)','lead(L=1)','subnum','select',round];
invest = array2table(behav_invest, 'VariableNames', title_beh);
writetable(invest, fullfile(basedir,'invest_single_trial_oneF.csv'),"Delimiter",",");
fprintf('Done!\n');