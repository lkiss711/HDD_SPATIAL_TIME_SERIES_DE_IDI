function [Pe,se]=PeriNormM(X,nfft,overlap)
%%% se is the noise variance
%  Pe is the normed periodogram
% X colums are the time series!! X=X(:);
sX=size(X);
lx=sX(1);
nadvance=fix(nfft*(1-overlap/100));
nrecs=fix((lx-nfft*overlap/100)/nadvance);%
Pe=zeros(nfft,sX(2));
locseg = 1:nfft;
for krec = 1:nrecs
  xseg   = X(locseg,:);
   %  xseg=detrend(x(locseg)); -mean(y(locseg)));
  Xf     = fft(xseg, nfft);
  Pe=Pe+abs(Xf).^2/nfft;
  locseg = locseg + nadvance;
end
Pe=Pe/nrecs;
Pe(1,:)= mean(Pe(2:3,:)); %% a folytonossag miatt 
se=exp(mean(log(Pe(1:nfft/2+1,:))));%
Pe=Pe./kron(ones(nfft,1),se);  % /2/pi
%spline(4/nfft:-1/nfft:1/nfft,fliplr(Pe(2:5)),0); 