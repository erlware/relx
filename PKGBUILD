# Maintainer: Aaron France <aaron.l.france@gmail.com

pkgname=relx
pkgrel=1
pkgver=1
pkgdesc="Sane, simple release creation for Erlang"
arch=(i686 x86_64)
license=('Apache')
url="https://github.com/erlware/relx"
depends=('erlang' 'rebar-git')
provides=("relx")
source=('git://github.com/erlware/relx.git')
makedepdends=('git')
md5sums=('SKIP')

_gitname='relx'

build() {
  cd "${srcdir}/${_gitname}"
  make
}

package() {
  cd "${srcdir}/${_gitname}"
  install -Dm755 relx "$pkgdir/usr/bin/relx"
}
