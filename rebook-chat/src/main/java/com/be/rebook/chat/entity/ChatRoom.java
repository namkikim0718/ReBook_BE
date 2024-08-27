package com.be.rebook.chat.entity;

import java.io.Serializable;
import java.util.List;

import com.be.rebook.chat.dto.ChatMessageDTO;
import com.be.rebook.chat.dto.CreateChatRoomDto;
import com.be.rebook.common.config.BaseEntity;

import jakarta.persistence.CascadeType;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.OneToMany;
import jakarta.persistence.OrderBy;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@Entity
@NoArgsConstructor
public class ChatRoom extends BaseEntity implements Serializable {

    private static final long serialVersionUID = 6494678977089006639L;

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "chat_room_id")
    private Long id;

    @Column(name = "buyer_username")
    private String buyerUsername;

    @Column(name = "seller_username")
    private String sellerUsername;

    @Column(name = "product_id")
    private Long productId;

    @OneToMany(mappedBy = "chatRoom", cascade = CascadeType.ALL, orphanRemoval = true)
    @OrderBy("createdAt ASC")
    private List<ChatMessage> messages;

    public ChatRoom(String buyerUsername, String sellerUsername, Long productId) {
        this.buyerUsername = buyerUsername;
        this.sellerUsername = sellerUsername;
        this.productId = productId;
    }

    public ChatRoom(CreateChatRoomDto createChatRoomDto) {
        this.buyerUsername = createChatRoomDto.getBuyerUsername();
        this.sellerUsername = createChatRoomDto.getSellerUsername();
        this.productId = createChatRoomDto.getProductId();
    }

    public void addMessage(ChatMessageDTO messageDto) {
        ChatMessage chatMessage = new ChatMessage(messageDto, this);
        this.messages.add(chatMessage);
    }

    public ChatMessage getLastMessage() {
        if (this.messages == null || this.messages.isEmpty()) {
            return null;
        }
        return this.messages.get(this.messages.size() - 1);
    }

    public String toString() {
        return "ChatRoom{" + "id=" + id + ", buyerUsername=" + buyerUsername + ", sellerUsername=" + sellerUsername
                + ", productId="
                + productId
                + ", messages=" + messages + '}';
    }
}