package com.be.rebook.product.entity;

import com.be.rebook.product.dto.ProductRequestDTO;
import com.be.rebook.product.type.ProductStatus;
import com.be.rebook.common.config.BaseEntity;
import jakarta.persistence.*;
import lombok.*;

import java.util.ArrayList;
import java.util.List;

@Entity
@Getter
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class Product extends BaseEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "product_id")
    private Long id;

    private String title;

    private String content;

    private int price;

    private String university;

    private String major;

    @Enumerated(EnumType.STRING)
    private ProductStatus status;

    private String sellerUsername;

    @OneToMany(mappedBy = "product", cascade = CascadeType.ALL, orphanRemoval = true)
    private List<ProductImage> productImages = new ArrayList<>();

    public static Product of(String sellerUsername, ProductRequestDTO.ProductSaveRequestDTO productSaveRequestDTO) {
        return builder()
                .title(productSaveRequestDTO.getTitle())
                .content(productSaveRequestDTO.getContent())
                .price(productSaveRequestDTO.getPrice())
                .university(productSaveRequestDTO.getUniversity())
                .major(productSaveRequestDTO.getMajor())
                .status(ProductStatus.PENDING)
                .sellerUsername(sellerUsername)
                .build();
    }

    /**
     * 상태변경 로직
     */
    public void changeStatus(String status) {
        if (status.equals("Completed")) {
            this.status = ProductStatus.COMPLETED;
        } else if (status.equals("Pending")) {
            this.status = ProductStatus.PENDING;
        }
    }
}

