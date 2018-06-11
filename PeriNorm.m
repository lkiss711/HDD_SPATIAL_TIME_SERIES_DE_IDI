function [Pe,se]=PeriNorm(x,nfft,overlap)
%%% se is the noise variance
%  Pe is the normed periodogram
x=x(:);
lx=length(x);
nadvance=fix(nfft*(1-overlap/100));
nrecs=fix((lx-nfft*overlap/100)/nadvance);%
Pe=zeros(nfft,1);
locseg = 1:nfft;
for krec = 1:nrecs
  xseg   = x(locseg);
   %  xseg=detrend(x(locseg)); -mean(y(locseg)));
  Xf     = fft(xseg, nfft);
  Pe=Pe+abs(Xf).^2/nfft;
  locseg = locseg + nadvance;
end
Pe=Pe/nrecs;
Pe(1)= mean(Pe(2:3)); %% a folytonossag miatt 
se=exp(mean(log(Pe(2:nfft))));%
Pe=Pe/se;  % /2/pi
%spline(4/nfft:-1/nfft:1/nfft,fliplr(Pe(2:5)),0); 