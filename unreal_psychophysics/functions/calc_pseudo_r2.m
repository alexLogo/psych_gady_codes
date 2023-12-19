function [pseudo_r2] = calc_pseudo_r2(result)
%calc_pseudo_r2 calculates the Mcfadden pR^2 that is based on deviance. 
% uses psignfit functions and is based on Helen & Adam's script
% see Helen's Sci. Rep 21 paper for equation
% adapted by YS 21/11/21
% INPUT result (struct) output of psignift function 
 
% calculate the deviance: 
                [devianceResiduals, deviance_right, samples_deviance, samples_devianceResiduals]...
                    = getDeviance(result,'');
                
                
                 % calculate the null deviance (based on the getDeviance function from psignifit):
                pPred_right = result.psiHandle(result.data(:,1));
                % change predicted to the null predicted: The mean of y values.
                pPred_right(1:end) = mean(result.data(:,2) ./ result.data(:,3));
                pMeasured_right = result.data(:,2) ./ result.data(:,3);
                loglikelihoodPred_right = result.data(:,2) .* log(pPred_right)...
                    + (result.data(:,3) - result.data(:,2)) .* log((1 - pPred_right));
                loglikelihoodMeasured_right = result.data(:,2) .* log(pMeasured_right)...
                    + (result.data(:,3) - result.data(:,2)) .* log((1 - pMeasured_right));
                loglikelihoodMeasured_right(pMeasured_right == 1) = 0;
                loglikelihoodMeasured_right(pMeasured_right == 0) = 0;
                devianceResiduals_right = -2*sign(pMeasured_right - pPred_right).*(loglikelihoodMeasured_right - loglikelihoodPred_right);
                deviance_null_right = sum(abs(devianceResiduals_right));
            pseudo_r2 =(deviance_null_right - deviance_right)/deviance_null_right;
 
end

