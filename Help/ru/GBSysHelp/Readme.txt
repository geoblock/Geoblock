Создание системной справки с ипользованием скрипта PasDoc
------------------------------------------------------------
Для создания файла справки GBSysHelp.chm выполните следующие шаги -
1.Загрузите программу PasDoc с сайта http://pasdoc.sourceforge.net/.
2.Запустите pasdoc_gui и откройте скрипт GBSysHelp.pds в этой же директории 
3.Сгенерируйте HTML файлы системной справки, щелкнув по кнопке Generate
4.Загрузите HTML Help Workshop 1.3 с сайта Microsoft и установите её.
5.Запустите программу hhw.exe в директории C:\Program Files (x86)\HTML Help Workshop\
6.Откройте проект GBSysHelp.hhp в папке ..\ru\GBSysHelp\HTML\ и скомпилируйте его
7.Там будет создан GBSysHelp.chm. Очистите папку, используя _CleanHTML.bat
