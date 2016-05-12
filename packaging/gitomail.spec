Name:           gitomail
Version:        PKG_VERSION
Release:        PKG_RELEASE%{?dist}
Summary:        Automatic email generation for Git with maintainership tracking
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

%if 0%{?fedora} >= 24
# GHC builds need tinfo.so.5
BuildRequires:  ncurses-compat-libs
BuildRequires:  glibc-langpack-en
%endif

%description
Automatic email generation for Git with maintainership tracking.

%global debug_package %{nil}

%prep
%setup -q

export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
export LANGUAGE=en_US.UTF-8

rm version.sh
cp %{_sourcedir}/Version.hs src/Gitomail/Version.hs
stack --no-terminal setup

%build

export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
export LANGUAGE=en_US.UTF-8

stack --no-terminal build

%install

mkdir -p $RPM_BUILD_ROOT/%{_prefix}/bin
cp .stack-work/install/*/*/*/bin/gitomail $RPM_BUILD_ROOT/%{_prefix}/bin/gitomail

%files
%{_prefix}/bin/gitomail

%changelog
