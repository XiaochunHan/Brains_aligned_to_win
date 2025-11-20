clear;clc;
basedir = '/Users/xiaochunhan/Documents/Research/Projects/Brains_aligned_to_win/Data/Develop';
files = {'BINS','BOLD','FC','INS'};
metafile = 'winlose_pay.mat';
%%
for f = 1:length(files)
    load(fullfile(basedir, [files{f},'_ind_data.mat']));
    data_invest = eval([files{f},'_data.inv']);
    data_outcome = eval([files{f},'_data.out']);
    varnames = eval([files{f},'_data.title']);
    output_invest = array2table(data_invest, 'VariableNames', varnames);
    output_outcome = array2table(data_outcome, 'VariableNames', varnames);
    writetable(output_invest, fullfile(basedir,[files{f},'_single_trial_invest.csv']),"Delimiter",",");
    writetable(output_outcome, fullfile(basedir,[files{f},'_single_trial_outcome.csv']),"Delimiter",",");
    clear data_invest data_outcome varnames output_invest output_outcome
end
%%
load(fullfile(basedir, metafile));
winpay = array2table(all, 'VariableNames', title);
writetable(winpay, fullfile(basedir,'win_lose_and_pay_single_trial.csv'),"Delimiter",",");

fprintf('Done!\n');