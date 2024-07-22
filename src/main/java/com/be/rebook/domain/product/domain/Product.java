package com.be.rebook.domain.product.domain;

import com.be.rebook.domain.productImage.domain.ProductImage;
import com.be.rebook.global.config.BaseEntity;
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

    private String isbn;

    private String bookTitle;

    private String author;

    private String publisher;

    private String publishDate;

    private String university;

    private String major;

    @Enumerated(EnumType.STRING)
    private ProductStatus status;
    /*
        @ManyToOne(fetch = FetchType.LAZY)
        @JoinColumn(name = "seller_id")
        private Member member;
    */
    @OneToMany(mappedBy = "product")
    private List<ProductImage> productImages = new ArrayList<>();
}

