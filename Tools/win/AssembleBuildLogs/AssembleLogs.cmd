if exist "%CONFIGURATIONBUILDDIR%\BuildOutput.htm" del "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"

if not exist "%CONFIGURATIONBUILDDIR%\obj%PlatformArchitecture%\preprocessor\BuildLog.htm" GOTO SkipANGLEProjects

echo _________________________________________________________ >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
echo COMPILING preprocessor...                                 >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
echo _________________________________________________________ >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
type "%CONFIGURATIONBUILDDIR%\obj%PlatformArchitecture%\preprocessor\BuildLog.htm" >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"

echo _________________________________________________________ >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
echo COMPILING translator_common...                            >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
echo _________________________________________________________ >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
type "%CONFIGURATIONBUILDDIR%\obj%PlatformArchitecture%\translator_common\BuildLog.htm" >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"

echo _________________________________________________________ >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
echo COMPILING translator_hlsl...                              >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
echo _________________________________________________________ >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
type "%CONFIGURATIONBUILDDIR%\obj%PlatformArchitecture%\translator_hlsl\BuildLog.htm" >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"

echo _________________________________________________________ >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
echo COMPILING translator_glsl...                              >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
echo _________________________________________________________ >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
type "%CONFIGURATIONBUILDDIR%\obj%PlatformArchitecture%\translator_glsl\BuildLog.htm" >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"

echo _________________________________________________________ >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
echo COMPILING libGLESv2...                                    >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
echo _________________________________________________________ >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
type "%CONFIGURATIONBUILDDIR%\obj%PlatformArchitecture%\libGLESv2\BuildLog.htm"    >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"

echo _________________________________________________________ >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
echo COMPILING libEGL...                                       >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
echo _________________________________________________________ >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
type "%CONFIGURATIONBUILDDIR%\obj%PlatformArchitecture%\libEGL\BuildLog.htm"       >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"

:SkipANGLEProjects

if not exist "%CONFIGURATIONBUILDDIR%\obj%PlatformArchitecture%\WTF\BuildLog.htm" GOTO SkipIndividualBuilds

echo _________________________________________________________ >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
echo COMPILING WTF...                                          >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
echo _________________________________________________________ >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
type "%CONFIGURATIONBUILDDIR%\obj%PlatformArchitecture%\WTF\BuildLog.htm" >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"

echo _________________________________________________________ >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
echo COMPILING JavaScriptCoreGenerated...                      >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
echo _________________________________________________________ >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
type "%CONFIGURATIONBUILDDIR%\obj%PlatformArchitecture%\JavaScriptCoreGenerated\BuildLog.htm" >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"

echo _________________________________________________________ >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
echo COMPILING LLIntDesiredOffsets...                          >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
echo _________________________________________________________ >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
type "%CONFIGURATIONBUILDDIR%\obj%PlatformArchitecture%\LLIntDesiredOffsets\BuildLog.htm" >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"

echo _________________________________________________________ >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
echo COMPILING LLIntOffsetsExtractor...                        >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
echo _________________________________________________________ >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
type "%CONFIGURATIONBUILDDIR%\obj%PlatformArchitecture%\LLIntOffsetsExtractor\BuildLog.htm" >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"

echo _________________________________________________________ >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
echo COMPILING LLIntAssembly...                                >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
echo _________________________________________________________ >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
type "%CONFIGURATIONBUILDDIR%\obj%PlatformArchitecture%\LLIntAssembly\BuildLog.htm" >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"

echo _________________________________________________________ >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
echo COMPILING JavaScriptCore...                               >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
echo _________________________________________________________ >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
type "%CONFIGURATIONBUILDDIR%\obj%PlatformArchitecture%\JavaScriptCore\BuildLog.htm" >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"

echo _________________________________________________________ >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
echo COMPILING jsc...                                          >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
echo _________________________________________________________ >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
type "%CONFIGURATIONBUILDDIR%\obj%PlatformArchitecture%\jsc\BuildLog.htm" >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"

echo _________________________________________________________ >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
echo COMPILING jscLauncher...                                  >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
echo _________________________________________________________ >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
type "%CONFIGURATIONBUILDDIR%\obj%PlatformArchitecture%\jsclauncher\BuildLog.htm" >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"

echo _________________________________________________________ >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
echo COMPILING testRegExp...                                   >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
echo _________________________________________________________ >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
type "%CONFIGURATIONBUILDDIR%\obj%PlatformArchitecture%\testRegExp\BuildLog.htm" >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"

echo _________________________________________________________ >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
echo COMPILING testRegExpLauncher...                           >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
echo _________________________________________________________ >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
type "%CONFIGURATIONBUILDDIR%\obj%PlatformArchitecture%\testRegExpLauncher\BuildLog.htm" >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"

echo _________________________________________________________ >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
echo COMPILING testapi...                                      >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
echo _________________________________________________________ >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
type "%CONFIGURATIONBUILDDIR%\obj%PlatformArchitecture%\testapi\BuildLog.htm" >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"

echo _________________________________________________________ >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
echo COMPILING testapiLauncher...                              >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
echo _________________________________________________________ >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
type "%CONFIGURATIONBUILDDIR%\obj%PlatformArchitecture%\testapiLauncher\BuildLog.htm" >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"

if not exist "%CONFIGURATIONBUILDDIR%\obj%PlatformArchitecture%\WebKitQuartzCoreAdditions\BuildLog.htm" GOTO SkipInternalProjects

echo _________________________________________________________ >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
echo COMPILING WebKitSystemInterfaceGenerated...               >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
echo _________________________________________________________ >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
type "%CONFIGURATIONBUILDDIR%\obj%PlatformArchitecture%\WebKitSystemInterfaceGenerated\BuildLog.htm" >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"

echo _________________________________________________________ >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
echo COMPILING WebKitSystemInterface...                        >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
echo _________________________________________________________ >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
type "%CONFIGURATIONBUILDDIR%\obj%PlatformArchitecture%\WebKitSystemInterface\BuildLog.htm" >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"

echo _________________________________________________________ >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
echo COMPILING WebKitQuartzCoreAdditionsGenerated...           >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
echo _________________________________________________________ >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
type "%CONFIGURATIONBUILDDIR%\obj%PlatformArchitecture%\WebKitQuartzCoreAdditionsGenerated\BuildLog.htm" >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"

echo _________________________________________________________ >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
echo COMPILING WebKitQuartzCoreAdditions...                    >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
echo _________________________________________________________ >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
type "%CONFIGURATIONBUILDDIR%\obj%PlatformArchitecture%\WebKitQuartzCoreAdditions\BuildLog.htm" >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"

:SkipInternalProjects

echo _________________________________________________________ >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
echo COMPILING WebCoreGenerated...                             >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
echo _________________________________________________________ >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
type "%CONFIGURATIONBUILDDIR%\obj%PlatformArchitecture%\WebCoreGenerated\BuildLog.htm" >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"

echo _________________________________________________________ >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
echo COMPILING WebCore...                                      >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
echo _________________________________________________________ >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
type "%CONFIGURATIONBUILDDIR%\obj%PlatformArchitecture%\WebCore\BuildLog.htm" >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"

echo _________________________________________________________ >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
echo COMPILING WebCoreTestSupport...                           >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
echo _________________________________________________________ >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
type "%CONFIGURATIONBUILDDIR%\obj%PlatformArchitecture%\WebCoreTestSupport\BuildLog.htm" >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"

echo _________________________________________________________ >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
echo COMPILING Interfaces...                                   >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
echo _________________________________________________________ >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
type "%CONFIGURATIONBUILDDIR%\obj%PlatformArchitecture%\Interfaces\BuildLog.htm" >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"

echo _________________________________________________________ >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
echo COMPILING WebKitGUID...                                   >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
echo _________________________________________________________ >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
type "%CONFIGURATIONBUILDDIR%\obj%PlatformArchitecture%\WebKitGUID\BuildLog.htm" >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"

echo _________________________________________________________ >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
echo COMPILING WebKit...                                       >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
echo _________________________________________________________ >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
type "%CONFIGURATIONBUILDDIR%\obj%PlatformArchitecture%\WebKit\BuildLog.htm" >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"

echo _________________________________________________________ >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
echo COMPILING WebInspectorUI...                               >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
echo _________________________________________________________ >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
type "%CONFIGURATIONBUILDDIR%\obj%PlatformArchitecture%\WebInspectorUI\BuildLog.htm" >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"

echo _________________________________________________________ >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
echo COMPILING MiniBrowserLib...                               >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
echo _________________________________________________________ >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
type "%CONFIGURATIONBUILDDIR%\obj%PlatformArchitecture%\MiniBrowserLib\BuildLog.htm" >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"

echo _________________________________________________________ >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
echo COMPILING MiniBrowser...                                  >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
echo _________________________________________________________ >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
type "%CONFIGURATIONBUILDDIR%\obj%PlatformArchitecture%\MiniBrowser\BuildLog.htm" >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"

echo _________________________________________________________ >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
echo COMPILING TestNetscapePlugin...                           >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
echo _________________________________________________________ >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
type "%CONFIGURATIONBUILDDIR%\obj%PlatformArchitecture%\TestNetscapePlugin\BuildLog.htm" >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"

echo _________________________________________________________ >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
echo COMPILING ImageDiff...                                    >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
echo _________________________________________________________ >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
type "%CONFIGURATIONBUILDDIR%\obj%PlatformArchitecture%\ImageDiff\BuildLog.htm" >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"

echo _________________________________________________________ >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
echo COMPILING ImageDiffLauncher...                            >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
echo _________________________________________________________ >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
type "%CONFIGURATIONBUILDDIR%\obj%PlatformArchitecture%\ImageDiffLauncher\BuildLog.htm" >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"

echo _________________________________________________________ >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
echo COMPILING DumpRenderTree...                               >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
echo _________________________________________________________ >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
type "%CONFIGURATIONBUILDDIR%\obj%PlatformArchitecture%\DumpRenderTree\BuildLog.htm" >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"

echo _________________________________________________________ >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
echo COMPILING DumpRenderTreeLauncher...                       >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
echo _________________________________________________________ >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
type "%CONFIGURATIONBUILDDIR%\obj%PlatformArchitecture%\DumpRenderTreeLauncher\BuildLog.htm" >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"

echo _________________________________________________________ >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
echo COMPILING record-memory...                                >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
echo _________________________________________________________ >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
type "%CONFIGURATIONBUILDDIR%\obj%PlatformArchitecture%\record-memory\BuildLog.htm" >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"

echo _________________________________________________________ >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
echo COMPILING gtest-md...                                     >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
echo _________________________________________________________ >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
type "%CONFIGURATIONBUILDDIR%\obj%PlatformArchitecture%\gtest-md\BuildLog.htm" >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"

echo _________________________________________________________ >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
echo COMPILING TestWebKitAPI...                                >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
echo _________________________________________________________ >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
type "%CONFIGURATIONBUILDDIR%\obj%PlatformArchitecture%\TestWebKitAPI\BuildLog.htm" >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"

:SkipIndividualBuilds

if not exist "%CONFIGURATIONBUILDDIR%\webkit_warnings.log" GOTO SkipMSBuildFiles

echo _________________________________________________________ >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
echo Build ERRORS...                                           >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
echo _________________________________________________________ >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
type "%CONFIGURATIONBUILDDIR%\webkit_errors.log" >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"

echo _________________________________________________________ >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
echo Build WARNINGS...                                         >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
echo _________________________________________________________ >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"
type "%CONFIGURATIONBUILDDIR%\webkit_warnings.log" >> "%CONFIGURATIONBUILDDIR%\BuildOutput.htm"

:SkipMSBuildFiles

echo "Completed."