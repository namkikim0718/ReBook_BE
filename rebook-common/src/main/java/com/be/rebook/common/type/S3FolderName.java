package com.be.rebook.common.type;

public enum S3FolderName {
    PROFILE("profile"),
    PRODUCT("product"),
    CHAT("chat");

    private final String folderName;

    S3FolderName(String folderName) {
        this.folderName = folderName;
    }

    public String getFolderName() {
        return folderName;
    }
}
