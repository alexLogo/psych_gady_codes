function [str] = print_thresh_values(vec)
%PRINT_THRESH_VALUES Summary of this function goes here
%   Detailed explanation goes here
%I changed the precision myself from 4 digits after the zero to 2 on 3rd of
%January 2022
str = sprintf(' |%.2f',vec);
str = str(2:end);
end


