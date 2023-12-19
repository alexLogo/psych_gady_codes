function [GC,GR] = groupcounts_YS(x)
%GROUPCOUNTS_YS  a homemade version of groupconts for older matlab versions
%   GR- unique x values
% GC- # of times they appear
GR=unique(x);
GC=[];
for i=1:length(GR)
    GC(i,1)=length(find(x==GR(i)));
end
end

