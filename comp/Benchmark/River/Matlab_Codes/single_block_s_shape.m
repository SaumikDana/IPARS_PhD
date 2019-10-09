clear
clc
max_ele = 10;           % maximum 1d elements in subdomain
max_nod = max_ele+1;    % maximum 1d nodes in subdomain
max_sub = 2;            % maximum number of subdomains

max_nod1 = 2*max_ele+1;
max_ele1 = 2*max_ele;

xyz111=zeros(3,max_sub);

dxhat=zeros(max_ele,max_sub);
dyhat=zeros(max_ele,max_sub);
dzhat=zeros(max_ele,max_sub);

xhat=zeros(max_nod,max_sub);
yhat=zeros(max_nod,max_sub);
zhat=zeros(max_nod,max_sub);

xhat1=zeros(max_nod1,1);
yhat1=zeros(max_nod1,1);
zhat1=zeros(max_nod1,1);

xphys1=zeros(max_nod1,max_nod1,max_nod1);
yphys1=zeros(max_nod1,max_nod1,max_nod1);
zphys1=zeros(max_nod1,max_nod1,max_nod1);

K=zeros(3,3,max_ele,max_ele,max_ele,max_sub);

nx=zeros(max_sub);
ny=zeros(max_sub);
nz=zeros(max_sub);

%----------------------------------------------------------------------

nsub = 2;

R1=50.;
R2=100.;
L=pi/2*(R1+R2);

Khat=[100,0,0; 0,10,0; 0,0,10];

%-----

isub=1;

xyz111(:,isub)=[0,R1,0];

nx(isub)=10;
ny(isub)=10;
nz(isub)=1;

wx=L/2;
wy=R2-R1;
wz=10;

dxhat(1:nx(isub),isub)=wx/nx(isub);
dyhat(1:ny(isub),isub)=wy/ny(isub);
dzhat(1:nz(isub),isub)=wz/nz(isub);

%-----

isub=2;

xyz111(:,isub)=[L/2,R1,0];

% nx(isub)=7;
% ny(isub)=7;
% nz(isub)=1;

nx(isub)=10;
ny(isub)=10;
nz(isub)=1;

wx=L/2;
wy=R2-R1;
wz=10;

dxhat(1:nx(isub),isub)=wx/nx(isub);
dyhat(1:ny(isub),isub)=wy/ny(isub);
dzhat(1:nz(isub),isub)=wz/nz(isub);

%-----


%----- Assumes domains are alinged in X-direction
nxtot = 0;
nytot = ny(1);
nztot = nz(1);
for i = 1:isub
    nxtot = nxtot + nx(isub);
end

mark1 = zeros(nxtot+1,nytot+1,nztot+1);
mark2 = zeros(nxtot,nytot,nztot);

for isub=1:nsub
    xhat(1,isub)=xyz111(1,isub);
    yhat(1,isub)=xyz111(2,isub);
    zhat(1,isub)=xyz111(3,isub);
    xhat1(1+nx(isub)*(isub-1))=xyz111(1,isub);
    yhat1(1)=xyz111(2,isub);
    zhat1(1)=xyz111(3,isub);
    for i=1:nx(isub)
        xhat(i+1,isub)=xhat(i,isub)+dxhat(i,isub);
        xhat1(i+1+nx(isub)*(isub-1)) = xhat(i+1,isub);
    end
    for i=1:ny(isub)
        yhat(i+1,isub)=yhat(i,isub)+dyhat(i,isub);
        yhat1(i+1) = yhat(i+1,isub);
    end
    for i=1:nz(isub)
        zhat(i+1,isub)=zhat(i,isub)+dzhat(i,isub);
        zhat1(i+1) = zhat(i+1,isub);
    end
end

for isub=1:nsub
    for i=1:nx(isub)+1
        for j=1:ny(isub)+1
            for k=1:nz(isub)+1
                mark1(i+nx(isub)*(isub-1),j,k)=isub;
            end
        end
    end
end

for isub=1:nsub
    for i=1:nx(isub)
        for j=1:ny(isub)
            for k=1:nz(isub)
                mark2(i+nx(isub)*(isub-1),j,k)=isub;
            end
        end
    end
end

for i=1:nxtot+1
    for j=1:nytot+1
        for k=1:nztot+1
            l = xhat1(i);
            r = yhat1(j);
            z = zhat1(k);           
            if (mark1(i,j,k)==1)
                theta=pi/2*(1-2*l/L);
                xphys1(i,j,k)=r*cos(theta);
                yphys1(i,j,k)=r*sin(theta);
                zphys1(i,j,k)=z;
            elseif (mark1(i,j,k)==2)
                theta=pi*(1/2+l/L);
                xphys1(i,j,k)=(R1+R2-r)*cos(theta)+R1+R2;
                yphys1(i,j,k)=(R1+R2-r)*sin(theta);
                zphys1(i,j,k)=z;
            end

        end
    end
end


for i=1:nxtot
    for j=1:nytot
        for k=1:nztot
            l1=(xhat1(i)+xhat1(i+1))/2;
            r1=(yhat1(j)+yhat1(j+1))/2;
            z1=(zhat1(k)+zhat1(k+1))/2;
            if (mark2(i,j,k)==1)
                phi=-pi*l1/L;
            elseif (mark2(i,j,k)==2)
                phi=pi/L*(l1-L);
            end
            rot=[cos(phi),-sin(phi),0; sin(phi),cos(phi),0; 0 0 1];
            K(:,:,i,j,k)=Khat*rot;
        end
    end
end


figure(1); hold off; plot(0,0); hold on;
figure(2); hold off; plot(0,0); hold on;

isub = 1;

m=nxtot*nytot*nztot;
n=(nxtot+1)*(nytot+1)*(nztot+1);
x=reshape(xphys1(1:nxtot+1,1:nytot+1,1:nztot+1),[n 1]);
y=reshape(yphys1(1:nxtot+1,1:nytot+1,1:nztot+1),[n 1]);
z=reshape(zphys1(1:nxtot+1,1:nytot+1,1:nztot+1),[n 1]);
xperm=reshape(K(1,1,1:nxtot,1:nytot,1:nztot),[m 1]);
yperm=reshape(K(2,2,1:nxtot,1:nytot,1:nztot),[m 1]);
zperm=reshape(K(3,3,1:nxtot,1:nytot,1:nztot),[m 1]);
xyperm=reshape(K(1,2,1:nxtot,1:nytot,1:nztot),[m 1]);
xzperm=reshape(K(1,3,1:nxtot,1:nytot,1:nztot),[m 1]);
yzperm=reshape(K(2,3,1:nxtot,1:nytot,1:nztot),[m 1]);
fprintf('BLOCKNAME(%1.1i) = "BLOCK %1.1i"\n',isub,isub);
fprintf('DOWN(1 TO 3,%1.1i) = 0 0 0\n',isub);
fprintf('NX(%1.1i)=%i  NY(%1.1i)=%i  NZ(%1.1i)=%i\n', ...
    isub,nxtot,isub,nytot,isub,nztot);
fprintf('XC%1.1i()=\n',isub);
fprintf('%15.6f %15.6f %15.6f %15.6f %15.6f %15.6f\n',x);
fprintf('\n');
fprintf('YC%1.1i()=\n',isub);
fprintf('%15.6f %15.6f %15.6f %15.6f %15.6f %15.6f\n',y);
fprintf('\n');
fprintf('ZC%1.1i()=\n',isub);
fprintf('%15.6f %15.6f %15.6f %15.6f %15.6f %15.6f\n',z);
fprintf('\n');
fprintf('XPERM%1.1i()=\n',isub);
fprintf('%15.6f %15.6f %15.6f %15.6f %15.6f %15.6f\n',xperm);
fprintf('\n');
fprintf('YPERM%1.1i()=\n',isub);
fprintf('%15.6f %15.6f %15.6f %15.6f %15.6f %15.6f\n',yperm);
fprintf('\n');
fprintf('ZPERM%1.1i()=\n',isub);
fprintf('%15.6f %15.6f %15.6f %15.6f %15.6f %15.6f\n',zperm);
fprintf('\n');
fprintf('XYPERM%1.1i()=\n',isub);
fprintf('%15.6f %15.6f %15.6f %15.6f %15.6f %15.6f\n',xyperm);
fprintf('\n');
fprintf('XZPERM%1.1i()=\n',isub);
fprintf('%15.6f %15.6f %15.6f %15.6f %15.6f %15.6f\n',xzperm);
fprintf('\n');
fprintf('YZPERM%1.1i()=\n',isub);
fprintf('%15.6f %15.6f %15.6f %15.6f %15.6f %15.6f\n',yzperm);
fprintf('\n\n');

figure(1);
X=reshape(xphys1(1:nxtot+1,1:nytot+1,1),[nxtot+1,nytot+1]);
Y=reshape(yphys1(1:nxtot+1,1:nytot+1,1),[nxtot+1,nytot+1]);
Z=zeros(size(X));
m=mesh(X,Y,Z);
set(m,'facecolor','none');
set(m,'edgecolor',[0 0 1]);

figure(2);
[X,Y]=meshgrid(xhat1(1:nxtot+1,1),yhat1(1:nytot+1,1));
Z=zeros(size(X));
m=mesh(X,Y,Z);
set(m,'facecolor','none');
set(m,'edgecolor',[1 0 0]);


figure(1); hold off;
figure(2); hold off;