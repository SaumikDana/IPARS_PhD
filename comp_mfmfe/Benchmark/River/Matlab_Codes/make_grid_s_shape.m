clear
clc
max_ele = 10;           % maximum 1d elements in subdomain
max_nod = max_ele+1;    % maximum 1d nodes in subdomain
max_sub = 2;            % maximum number of subdomains

xyz111=zeros(3,max_sub);

dxhat=zeros(max_ele,max_sub);
dyhat=zeros(max_ele,max_sub);
dzhat=zeros(max_ele,max_sub);

xhat=zeros(max_nod,max_sub);
yhat=zeros(max_nod,max_sub);
zhat=zeros(max_nod,max_sub);

xphys=zeros(max_nod,max_nod,max_nod,max_sub);
yphys=zeros(max_nod,max_nod,max_nod,max_sub);
zphys=zeros(max_nod,max_nod,max_nod,max_sub);

K=zeros(3,3,max_ele,max_ele,max_ele,max_sub);

nx=zeros(max_sub);
ny=zeros(max_sub);
nz=zeros(max_sub);

%----------------------------------------------------------------------

nsub = 2;

R1=50.;
R2=150.;
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

for isub=1:nsub

    xhat(1,isub)=xyz111(1,isub);
    yhat(1,isub)=xyz111(2,isub);
    zhat(1,isub)=xyz111(3,isub);

    for i=1:nx(isub)
        xhat(i+1,isub)=xhat(i,isub)+dxhat(i,isub);
    end

    for i=1:ny(isub)
        yhat(i+1,isub)=yhat(i,isub)+dyhat(i,isub);
    end

    for i=1:nz(isub)
        zhat(i+1,isub)=zhat(i,isub)+dzhat(i,isub);
    end
    
    for i=1:nx(isub)+1
        for j=1:ny(isub)+1
            for k=1:nz(isub)+1
                l=xhat(i,isub);
                r=yhat(j,isub);
                z=zhat(k,isub);
                if (isub==1)
                    theta=pi/2*(1-2*l/L);
                     xphys(i,j,k,isub)=r*cos(theta);
                     yphys(i,j,k,isub)=r*sin(theta);
                     zphys(i,j,k,isub)=z;    
%                    xphys(i,j,k,isub)=l;
%                    yphys(i,j,k,isub)=r;
%                    zphys(i,j,k,isub)=z;    
                else
                    theta=pi*(1/2+l/L);
                     xphys(i,j,k,isub)=(R1+R2-r)*cos(theta)+R1+R2;
                     yphys(i,j,k,isub)=(R1+R2-r)*sin(theta);
                     zphys(i,j,k,isub)=z; 
%                    xphys(i,j,k,isub)=l;
%                    yphys(i,j,k,isub)=r;
%                    zphys(i,j,k,isub)=z; 
                end
   
            end
        end
    end
    
    for i=1:nx(isub)
        for j=1:ny(isub)
            for k=1:nz(isub)
                l=(xhat(i,isub)+xhat(i+1,isub))/2;
                r=(yhat(j,isub)+yhat(j+1,isub))/2;
                z=(zhat(k,isub)+zhat(k+1,isub))/2;
                if (isub==1)
                    phi=-pi*l/L;
                else
                    phi=pi/L*(l-L);
                end
                rot=[cos(phi),-sin(phi),0; sin(phi),cos(phi),0; 0 0 1];
                K(:,:,i,j,k,isub)=Khat*rot;
%                K(:,:,i,j,k,isub)=Khat;
            end
        end
    end
    
end

figure(1); hold off; plot(0,0); hold on;
figure(2); hold off; plot(0,0); hold on;

for isub=1:nsub
    m=nx(isub)*ny(isub)*nz(isub);
    n=(nx(isub)+1)*(ny(isub)+1)*(nz(isub)+1);
    x=reshape(xphys(1:nx(isub)+1,1:ny(isub)+1,1:nz(isub)+1,isub),[n 1]);
    y=reshape(yphys(1:nx(isub)+1,1:ny(isub)+1,1:nz(isub)+1,isub),[n 1]);
    z=reshape(zphys(1:nx(isub)+1,1:ny(isub)+1,1:nz(isub)+1,isub),[n 1]);
    xperm=reshape(K(1,1,1:nx(isub),1:ny(isub),1:nz(isub),isub),[m 1]);
    yperm=reshape(K(2,2,1:nx(isub),1:ny(isub),1:nz(isub),isub),[m 1]);
    zperm=reshape(K(3,3,1:nx(isub),1:ny(isub),1:nz(isub),isub),[m 1]);
    xyperm=reshape(K(1,2,1:nx(isub),1:ny(isub),1:nz(isub),isub),[m 1]);
    xzperm=reshape(K(1,3,1:nx(isub),1:ny(isub),1:nz(isub),isub),[m 1]);
    yzperm=reshape(K(2,3,1:nx(isub),1:ny(isub),1:nz(isub),isub),[m 1]);
    fprintf('BLOCKNAME(%1.1i) = "BLOCK %1.1i"\n',isub,isub);
    fprintf('DOWN(1 TO 3,%1.1i) = 0 0 0\n',isub);
    fprintf('NX(%1.1i)=%i  NY(%1.1i)=%i  NZ(%1.1i)=%i\n', ...
        isub,nx(isub),isub,ny(isub),isub,nz(isub));
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
    X=reshape(xphys(1:nx(isub)+1,1:ny(isub)+1,1,isub),[nx(isub)+1,ny(isub)+1]);
    Y=reshape(yphys(1:nx(isub)+1,1:ny(isub)+1,1,isub),[nx(isub)+1,ny(isub)+1]);
    Z=zeros(size(X));
    m=mesh(X,Y,Z);
    set(m,'facecolor','none');
    set(m,'edgecolor',[0 0 1]);

    figure(2);
    [X,Y]=meshgrid(xhat(1:nx(isub)+1,isub),yhat(1:ny(isub)+1,isub));
    Z=zeros(size(X));
    m=mesh(X,Y,Z);
    set(m,'facecolor','none');
    set(m,'edgecolor',[1 0 0]);
end

figure(1); hold off;
figure(2); hold off;