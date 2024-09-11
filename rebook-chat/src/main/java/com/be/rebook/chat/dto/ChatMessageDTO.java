package com.be.rebook.chat.dto;

import com.be.rebook.chat.entity.ChatMessage;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@AllArgsConstructor
public class ChatMessageDTO { // TODO : ChatMessage 추상화
    private String senderUsername;
    private MessageType type;
    private String message;
    private Long roomId;

    // READ 전용
    private Long chatMessageId;
    private boolean isRead;

    // 메시지 생성시각
    private String createdAt;

    // 경고 메시지
    private String warningMessage;

    public enum MessageType {
        JOIN, TALK, ENTER, READ, ERROR
    }

    public ChatMessageDTO(ChatMessage message) {
        this.senderUsername = message.getSenderUsername();
        this.type = ChatMessageDTO.MessageType.TALK;
        this.message = message.getMessage();
        this.roomId = message.getChatRoom().getId();
        this.isRead = message.getIsRead();
        this.chatMessageId = message.getId();
        this.createdAt = message.getCreatedAt().toString();
        this.warningMessage = message.getWarningMessage();
    }

    public void setWarningMessage(String warningMessage) {
        this.warningMessage = warningMessage;
    }

    @Override
    public String toString() {
        return "ChatMessageDTO{" +
                "senderUsername=" + senderUsername +
                ", type=" + type +
                ", message='" + message + '\'' +
                ", roomId=" + roomId +
                '}';
    }
}
