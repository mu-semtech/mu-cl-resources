FROM madnificent/lisp-webservice

COPY . /app
COPY load-config.sh /load-config.sh

ENV BOOT=mu-cl-resources
RUN mkdir /config
RUN sbcl --load /usr/src/load.lisp

CMD sh /load-config.sh; sbcl --load /usr/src/startup.lisp
