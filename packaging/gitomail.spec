Name:           gitomail
Version:        PKG_VERSION
Release:        PKG_RELEASE%{?dist}
Summary:        Automatic E-Mail generation for Git with maintainership tracking
Group:          System Environment/Development Tools
License:        GPLv2
URL:            https://github.com/kernelim/gitomail
Source0:        %{name}-%{version}.tar.gz
Source1:        Version.hs
BuildRoot:      %(mktemp -ud %{_tmppath}/%{name}-%{version}-%{release}-XXXXXX)

BuildRequires:  stack
BuildRequires:  ghc
BuildRequires:  git
BuildRequires:  libicu-devel
BuildRequires:  zlib-devel
BuildRequires:  snappy-devel
BuildRequires:  openssl-devel
BuildRequires:  gmp-devel
BuildRequires:  pcre-devel

%description
Automatic E-Mail generation for Git with maintainership tracking.

%global debug_package %{nil}

%prep
%setup -q

rm version.sh
cp %{_sourcedir}/Version.hs src/Gitomail/Version.hs
stack --no-terminal setup

%build
stack --no-terminal build

%install

mkdir -p $RPM_BUILD_ROOT/%{_prefix}/bin
cp .stack-work/install/*/*/*/bin/gitomail $RPM_BUILD_ROOT/%{_prefix}/bin/gitomail

%files
%{_prefix}/bin/gitomail

%changelog
