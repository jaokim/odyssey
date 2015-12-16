/*
 * Copyright (C) 2006 Apple Inc.  All rights reserved.
 * Copyright (C) 2010 Igalia S.L
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY APPLE INC. ``AS IS'' AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL APPLE INC. OR
 * CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY
 * OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
 */

#ifndef ContextMenuItem_h
#define ContextMenuItem_h

#if ENABLE(CONTEXT_MENUS)

#include "PlatformMenuDescription.h"
#include <wtf/text/WTFString.h>

#if PLATFORM(COCOA)
#include <wtf/RetainPtr.h>
OBJC_CLASS NSImage;
OBJC_CLASS NSMenuItem;
#elif PLATFORM(GTK)
typedef struct _GtkMenuItem GtkMenuItem;
typedef struct _GtkAction GtkAction;
#elif PLATFORM(MUI)
#include "BALBase.h"
#endif
#endif // ENABLE(CONTEXT_MENUS)

namespace WebCore {

    class ContextMenu;
    class Image;
    class URL;

    // This enum needs to be in sync with the WebMenuItemTag enum in WebUIDelegate.h and the
    // extra values in WebUIDelegatePrivate.h
    enum ContextMenuAction {
        ContextMenuItemTagNoAction=0, // This item is not actually in WebUIDelegate.h
        ContextMenuItemTagOpenLinkInNewWindow=1,
        ContextMenuItemTagDownloadLinkToDisk,
        ContextMenuItemTagCopyLinkToClipboard,
        ContextMenuItemTagOpenImageInNewWindow,
        ContextMenuItemTagDownloadImageToDisk,
        ContextMenuItemTagCopyImageToClipboard,
#if PLATFORM(GTK) || PLATFORM(EFL)
        ContextMenuItemTagCopyImageUrlToClipboard,
#endif
        ContextMenuItemTagOpenFrameInNewWindow,
        ContextMenuItemTagCopy,
        ContextMenuItemTagGoBack,
        ContextMenuItemTagGoForward,
        ContextMenuItemTagStop,
        ContextMenuItemTagReload,
        ContextMenuItemTagCut,
        ContextMenuItemTagPaste,
#if PLATFORM(GTK)
        ContextMenuItemTagDelete,
#endif
#if PLATFORM(GTK) || PLATFORM (EFL)
        ContextMenuItemTagSelectAll,
#endif
#if PLATFORM(GTK)
        ContextMenuItemTagInputMethods,
        ContextMenuItemTagUnicode,
        ContextMenuItemTagUnicodeInsertLRMMark,
        ContextMenuItemTagUnicodeInsertRLMMark,
        ContextMenuItemTagUnicodeInsertLREMark,
        ContextMenuItemTagUnicodeInsertRLEMark,
        ContextMenuItemTagUnicodeInsertLROMark,
        ContextMenuItemTagUnicodeInsertRLOMark,
        ContextMenuItemTagUnicodeInsertPDFMark,
        ContextMenuItemTagUnicodeInsertZWSMark,
        ContextMenuItemTagUnicodeInsertZWJMark,
        ContextMenuItemTagUnicodeInsertZWNJMark,
#endif
        ContextMenuItemTagSpellingGuess,
        ContextMenuItemTagNoGuessesFound,
        ContextMenuItemTagIgnoreSpelling,
        ContextMenuItemTagLearnSpelling,
        ContextMenuItemTagOther,
        ContextMenuItemTagSearchInSpotlight,
        ContextMenuItemTagSearchWeb,
        ContextMenuItemTagLookUpInDictionary,
        ContextMenuItemTagOpenWithDefaultApplication,
        ContextMenuItemPDFActualSize,
        ContextMenuItemPDFZoomIn,
        ContextMenuItemPDFZoomOut,
        ContextMenuItemPDFAutoSize,
        ContextMenuItemPDFSinglePage,
        ContextMenuItemPDFFacingPages,
        ContextMenuItemPDFContinuous,
        ContextMenuItemPDFNextPage,
        ContextMenuItemPDFPreviousPage,
        // These are new tags! Not a part of API!!!!
        ContextMenuItemTagOpenLink = 2000,
        ContextMenuItemTagIgnoreGrammar,
        ContextMenuItemTagSpellingMenu, // Spelling or Spelling/Grammar sub-menu
        ContextMenuItemTagShowSpellingPanel,
        ContextMenuItemTagCheckSpelling,
        ContextMenuItemTagCheckSpellingWhileTyping,
        ContextMenuItemTagCheckGrammarWithSpelling,
        ContextMenuItemTagFontMenu, // Font sub-menu
        ContextMenuItemTagShowFonts,
        ContextMenuItemTagBold,
        ContextMenuItemTagItalic,
        ContextMenuItemTagUnderline,
        ContextMenuItemTagOutline,
        ContextMenuItemTagStyles,
        ContextMenuItemTagShowColors,
        ContextMenuItemTagSpeechMenu, // Speech sub-menu
        ContextMenuItemTagStartSpeaking,
        ContextMenuItemTagStopSpeaking,
        ContextMenuItemTagWritingDirectionMenu, // Writing Direction sub-menu
        ContextMenuItemTagDefaultDirection,
        ContextMenuItemTagLeftToRight,
        ContextMenuItemTagRightToLeft,
        ContextMenuItemTagPDFSinglePageScrolling,
        ContextMenuItemTagPDFFacingPagesScrolling,
        ContextMenuItemTagInspectElement,
        ContextMenuItemTagTextDirectionMenu, // Text Direction sub-menu
        ContextMenuItemTagTextDirectionDefault,
        ContextMenuItemTagTextDirectionLeftToRight,
        ContextMenuItemTagTextDirectionRightToLeft,
#if PLATFORM(COCOA)
        ContextMenuItemTagCorrectSpellingAutomatically,
        ContextMenuItemTagSubstitutionsMenu,
        ContextMenuItemTagShowSubstitutions,
        ContextMenuItemTagSmartCopyPaste,
        ContextMenuItemTagSmartQuotes,
        ContextMenuItemTagSmartDashes,
        ContextMenuItemTagSmartLinks,
        ContextMenuItemTagTextReplacement,
        ContextMenuItemTagTransformationsMenu,
        ContextMenuItemTagMakeUpperCase,
        ContextMenuItemTagMakeLowerCase,
        ContextMenuItemTagCapitalize,
        ContextMenuItemTagChangeBack,
#endif
        ContextMenuItemTagOpenMediaInNewWindow,
        ContextMenuItemTagDownloadMediaToDisk,
        ContextMenuItemTagCopyMediaLinkToClipboard,
        ContextMenuItemTagToggleMediaControls,
        ContextMenuItemTagToggleMediaLoop,
        ContextMenuItemTagEnterVideoFullscreen,
        ContextMenuItemTagMediaPlayPause,
        ContextMenuItemTagMediaMute,
        ContextMenuItemTagDictationAlternative,
        ContextMenuItemTagOpenLinkInThisWindow,
        ContextMenuItemTagToggleVideoFullscreen,
        ContextMenuItemTagShareMenu, 
        ContextMenuItemBaseCustomTag = 5000,
        ContextMenuItemCustomTagNoAction = 5998,
        ContextMenuItemLastCustomTag = 5999,
        ContextMenuItemBaseApplicationTag = 10000
    };

    enum ContextMenuItemType {
        ActionType,
        CheckableActionType,
        SeparatorType,
        SubmenuType
    };

#if ENABLE(CONTEXT_MENUS)
#if PLATFORM(COCOA)
    typedef NSMenuItem* PlatformMenuItemDescription;
#elif PLATFORM(GTK)
    typedef GtkMenuItem* PlatformMenuItemDescription;
#elif PLATFORM(MUI)
    struct PlatformMenuItemDescription {
        PlatformMenuItemDescription()
            : type(ActionType)
            , action(ContextMenuItemTagNoAction)
            , subMenu(0)
            , checked(false)
            , enabled(true)
        {}

        ContextMenuItemType type;
        ContextMenuAction action;
        String title;
        BalMenu* subMenu;
        bool checked;
        bool enabled;
    };
#else
    typedef void* PlatformMenuItemDescription;
#endif

    class ContextMenuItem {
        WTF_MAKE_FAST_ALLOCATED;
    public:
        WEBCORE_EXPORT ContextMenuItem(ContextMenuItemType, ContextMenuAction, const String&, ContextMenu* subMenu = 0);
        WEBCORE_EXPORT ContextMenuItem(ContextMenuItemType, ContextMenuAction, const String&, bool enabled, bool checked);
#if PLATFORM(MUI)
        WEBCORE_EXPORT ContextMenuItem(BalMenuItem*);
#endif
        WEBCORE_EXPORT ~ContextMenuItem();

        void setType(ContextMenuItemType);
        WEBCORE_EXPORT ContextMenuItemType type() const;

        void setAction(ContextMenuAction);
        WEBCORE_EXPORT ContextMenuAction action() const;

        void setChecked(bool = true);
        WEBCORE_EXPORT bool checked() const;

        void setEnabled(bool = true);
        WEBCORE_EXPORT bool enabled() const;

        void setSubMenu(ContextMenu*);

#if PLATFORM(MAC)
        WEBCORE_EXPORT static ContextMenuItem shareMenuItem(const URL& absoluteLinkURL, const URL& downloadableMediaURL, NSImage *, const String& selectedText);
#endif

#if PLATFORM(GTK)
        GtkAction* gtkAction() const;
#endif

#if PLATFORM(MUI)
        static BalMenuItem* createNativeMenuItem(const PlatformMenuItemDescription&);
#endif

#if USE(CROSS_PLATFORM_CONTEXT_MENUS)
        ContextMenuItem(ContextMenuAction, const String&, bool enabled, bool checked, const Vector<ContextMenuItem>& subMenuItems);
        explicit ContextMenuItem(const PlatformContextMenuItem&);
        ContextMenuItem();

        bool isNull() const;

        // On Windows, the title (dwTypeData of the MENUITEMINFO) is not set in this function. Callers can set the title themselves,
        // and handle the lifetime of the title, if they need it.
        PlatformContextMenuItem platformContextMenuItem() const;

        void setTitle(const String& title) { m_title = title; }
        const String& title() const { return m_title; }

        const Vector<ContextMenuItem>& subMenuItems() const { return m_subMenuItems; }
#else
    public:
        WEBCORE_EXPORT explicit ContextMenuItem(PlatformMenuItemDescription);
        explicit ContextMenuItem(ContextMenu* subMenu);
        ContextMenuItem(ContextMenuAction, const String&, bool enabled, bool checked, Vector<ContextMenuItem>& submenuItems);
        WEBCORE_EXPORT ContextMenuItem();

#if PLATFORM(MUI)
        bool isNull() const { return m_platformDescription.action == ContextMenuItemTagNoAction; }
#else
        bool isNull() const { return !m_platformDescription; }
#endif

#if PLATFORM(GTK) || PLATFORM(MUI)
        WEBCORE_EXPORT PlatformMenuItemDescription releasePlatformDescription();
#endif

        WEBCORE_EXPORT PlatformMenuItemDescription platformDescription() const;

        WEBCORE_EXPORT String title() const;
        void setTitle(const String&);

        WEBCORE_EXPORT PlatformMenuDescription platformSubMenu() const;
        void setSubMenu(Vector<ContextMenuItem>&);

#endif // USE(CROSS_PLATFORM_CONTEXT_MENUS)
    private:
#if USE(CROSS_PLATFORM_CONTEXT_MENUS)
        ContextMenuItemType m_type;
        ContextMenuAction m_action;
        String m_title;
        bool m_enabled;
        bool m_checked;
        Vector<ContextMenuItem> m_subMenuItems;
#else
#if PLATFORM(COCOA)
        RetainPtr<NSMenuItem> m_platformDescription;
#else
        PlatformMenuItemDescription m_platformDescription;
#endif
#endif // USE(CROSS_PLATFORM_CONTEXT_MENUS)
    };

#endif // ENABLE(CONTEXT_MENUS)
}

#endif // ContextMenuItem_h
