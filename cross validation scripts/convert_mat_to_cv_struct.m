folder = '/path/to/python/generated/mat/files/';
TrainInd_path = strcat(folder,'TrainInd.mat');
load(TrainInd_path);
TestInd_path = strcat(folder,'TestInd.mat');
load(TestInd_path);
TrainLabel_path = strcat(folder,'TrainLabel.mat');
load(TrainLabel_path);
TestLabel_path = strcat(folder,'TestLabel.mat');
load(TestLabel_path);
cvin_path = strcat(folder,'cvin.mat');
load(cvin_path);

p2 = 10;
cv2 = 11;
p1 = 1;
cv1 = 10;

Train_cv2 = cell(p2,cv2);
Test_cv2 = cell(p2,cv2);
class_cv2 = cell(p2,cv2);
class_new_cv2 = cell(p2,cv2);

for p = 1:p2
    for f = 1:cv2
        tr_ind = double(squeeze(Train_cv2_raw(p,f,:)));
        ts_ind = double(squeeze(Test_cv2_raw(p,f,:)));
        tr_label = double(squeeze(Train_cv2_label(p,f,:)));
        tr_label(tr_label==2) = -1;
        ts_label = double(squeeze(Test_cv2_label(p,f,:)));
        ts_label(ts_label==2) = -1;
        Train_cv2{p,f} = tr_ind;
        Test_cv2{p,f} = ts_ind;
        class = {};
        class.groups = [1,2];
        class.groupdesc = 'ASD vs TD';
        class.ind = tr_ind;
        class.label = tr_label;
        
        class_new = {};
        class_new.groups = [1,2];
        class_new.groupdesc = 'ASD vs TD';
        class_new.ind = (1:length(ts_ind));
        class_new.label = ts_label;
        
        cv1_temp = cv1_all{p,f};
        Train_cv1_temp = cell(p1,cv1);
        Test_cv1_temp = cell(p1,cv1);
        class.TrainInd = cell(p1,cv1);
        class.TestInd = cell(p1,cv1);
        class.TrainLabel = cell(p1,cv1);
        class.TestLabel = cell(p1,cv1);
        
        for p_in = 1:p1
            for f_in = 1:cv1
                tr_in_ind = double(squeeze(cv1_temp.TrainInd(p_in,f_in,:)));
                ts_in_ind = double(squeeze(cv1_temp.TestInd(p_in,f_in,:)));
                Train_cv1_temp{p_in,f_in} = tr_in_ind;
                Test_cv1_temp{p_in,f_in} = ts_in_ind;
                class.TrainInd{p_in,f_in} = tr_in_ind;
                class.TestInd{p_in,f_in} = ts_in_ind;
                tr_in_label = double(squeeze(cv1_temp.TrainLabel(p_in,f_in,:)));
                tr_in_label(tr_in_label==2) = -1;
                ts_in_label = double(squeeze(cv1_temp.TestLabel(p_in,f_in,:)));
                ts_in_label(ts_in_label==2) = -1;
                class.TrainLabel{p_in,f_in} = tr_in_label;
                class.TestLabel{p_in,f_in} = ts_in_label;
            end
        end
        cv1_all{p,f}.TrainInd = Train_cv1_temp;
        cv1_all{p,f}.TestInd = Test_cv1_temp;
        fields = {'TrainLabel','TestLabel'};
        cv1_all{p,f} = rmfield(cv1_all{p,f},fields);
        class_cv2{p,f} = cell(1,1);
        class_cv2{p,f}{1,1} = class;
        class_new_cv2{p,f} = cell(1,1);
        class_new_cv2{p,f}{1,1} = class_new;
    end
end

clearvars f p f_in p_in cv1_temp Train_cv1_temp Test_cv1_temp class class_new cv1_temp_label tr_in_ind tr_ind ts_in_ind ts_ind ;
clearvars p2 cv2 p1 cv1 folder Train_cv2_raw TrainInd_path Test_cv2_raw TestInd_path cvin_path fields;
clearvars TrainLabel_path Train_cv2_label TestLabel_path Test_cv2_label tr_label tr_in_label ts_label ts_in_label;

cv = {};
cv.TrainInd = Train_cv2;
cv.TestInd = Test_cv2;
cv.class = class_cv2;
cv.cvin = cv1_all;
cv.classnew = class_new_cv2;

clearvars Train_cv2 Test_cv2 class_cv2 cv1_all class_new_cv2;