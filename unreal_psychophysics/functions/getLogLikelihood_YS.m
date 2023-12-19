function [mle] = getLogLikelihood_YS(result)
% MODIFIED BY YONI TO ONLY GET AIC
%getInformationCriteria(result) compute Akaike Information Criterion (AIC) and Watanabe-Akaike
%(Widely-Applicable) Information Criterion (WAIC) for use in model comparison with other
%psychometric models. See [1] for details of each. See also @getDeviance.
%
%[aic,waic] = getInformationCriteria(result)
%
% [1] Gelman, A., Hwang, J., & Vehtari, A. (2014). Understanding predictive information criteria for
% Bayesian models. Statistics and Computing, 24(6), 997–1016.
% https://doi.org/10.1007/s11222-013-9416-2

%% AIC (but using MAP rather than MLE)

% get log likelihood of data under maximum likelihood parameters
switch lower(result.options.estimateType)
    case {'mle', 'map'}
        map_result = result;
    otherwise
        % 'result' is something else... run MAP fitting anew
        map_options = result.options;
        map_options.estimateType = 'MAP';
        map_result = psignifit(result.data, map_options);
end

X = map_result.Fit;
[mle, mle_per] = logLikelihood(map_result.data, map_result.options, X(1), X(2), X(3), X(4), X(5));


end

